import hudson.tasks.test.AbstractTestResultAction

def isWindows(){
  //If NODE_LABELS environment variable is null, we assume we are on master unix machine
  if (env.NODE_LABELS == null) {
    return false
  }
    return env.NODE_LABELS.toLowerCase().contains('windows')
}

// Answers if we are in a development branch (we assume is a development branch if it starts with "Pharo")
def isDevelopmentBranch() {
	def branchName =  env.BRANCH_NAME
	def baseName = branchName.substring(0, 5)

	return baseName == "Pharo"
}

// Extracts Pharo version from the development branch (if it is "Pharo7.0" it will answer "7.0")
def getPharoVersionFromBranch() {
	def branchName =  env.BRANCH_NAME
	return branchName.substring(5)
}

def shell(params){
    if(isWindows()) bat(params)
    else sh(params)
}

def runTests(architecture){
  cleanWs()
  dir(env.STAGE_NAME) {
    try {
        unstash "bootstrap${architecture}"
        shell "bash -c 'bootstrap/scripts/runTests.sh ${architecture} ${env.STAGE_NAME}'"
        junit allowEmptyResults: true, testResults: "${env.STAGE_NAME}*.xml"
    } finally {
        archiveArtifacts allowEmptyArchive: true, artifacts: "${env.STAGE_NAME}*.xml", fingerprint: true
        archiveArtifacts allowEmptyArchive: true, artifacts: "*.fuel", fingerprint: true
        // I am archiving the logs to check for crashes and errors.
        if(fileExists('PharoDebug.log')){
            shell "mv PharoDebug.log PharoDebug-${env.STAGE_NAME}.log"
            archiveArtifacts allowEmptyArchive: true, artifacts: "PharoDebug-${env.STAGE_NAME}.log", fingerprint: true
        }
        if(fileExists('crash.dmp')){
            shell "mv crash.dmp crash-${env.STAGE_NAME}.dmp"
            archiveArtifacts allowEmptyArchive: true, artifacts: "crash-${env.STAGE_NAME}.dmp", fingerprint: true
        }
        if(fileExists('progress.log')){
            shell "mv progress.log progress-${env.STAGE_NAME}.log"
            shell "cat progress-${env.STAGE_NAME}.log"
            archiveArtifacts allowEmptyArchive: true, artifacts: "progress-${env.STAGE_NAME}.log", fingerprint: true
        }
    }
  }
}

def runCommandLineTests(){
  cleanWs()
  dir('cli-tests') {
    try {
        unstash "bootstrap64"
        shell "bash -c 'bootstrap/scripts/runPharoCommandLineTests.sh'"
        junit allowEmptyResults: true, testResults: "report.xml"
    } finally {
        archiveArtifacts allowEmptyArchive: true, artifacts: "report.xml", fingerprint: true
    }
  }
}

def shellOutput(params){
  return (isWindows())? bat(returnStdout: true, script: params).trim() : sh(returnStdout: true, script: params).trim()
}

def notifyBuild(status){
  node('unix'){ stage('notify'){
  try{

  //If this is development, we send the email to the beta list
  def toMail = "log-ci-beta@lists.pharo.org"
  def buildKind = env.BRANCH_NAME
  if (env.CHANGE_ID != null){
    buildKind = "PR ${env.CHANGE_ID}"
  }
  if( isDevelopmentBranch() ) {
    toMail = "log-ci@lists.pharo.org"
    buildKind = getPharoVersionFromBranch()
  }

  //We checkout scm to have access to the log information
  checkout scm
  def owner = "pharo-project"
  def title = status

  //Get the merge information from the last commit
  def logMessage = shellOutput('git log -1 --format="%B"')
  def logSHA = shellOutput('git log -1 --format="%p"')

  def mailMessage = "Could not extract further issue information from commit message: ${logMessage}"

  //If there is no pull request information, we will send a log with the last commit message only
  def isPRMergeCommit = logMessage.startsWith("Merge pull request ")
  if (isPRMergeCommit) {
    def pullRequestId = logMessage.split(' ')[3].substring(1)
    def githubPullRequestHttpRequest = "https://api.github.com/repos/${owner}/pharo/pulls/${pullRequestId}"
    def response = httpRequest githubPullRequestHttpRequest
    if (response.status == 200) {
      def pullRequestJSON = readJSON text: response.content
      def pullRequestTitle = pullRequestJSON['title']

      def pullRequestUrl = "https://github.com/${owner}/pharo/pull/${pullRequestId}"
      mailMessage = """The Pull Request #${pullRequestId} was integrated: \"${pullRequestTitle}\"
Pull request url: ${pullRequestUrl}
"""
      title = pullRequestTitle
      def issueNumber = pullRequestJSON['head']['ref'].split('-')[0]
      def issueUrl = "https://github.com/pharo-project/pharo/issues/${issueNumber}"

      mailMessage += """
Issue Url: ${issueUrl}"""
    } else {
      mailMessage += """
No associated issue found"""
    }
  }

  def body = """There is a new Pharo build available!

The status of the build #${env.BUILD_NUMBER} was: ${status}.

${mailMessage}
Build Url: ${env.BUILD_URL}
"""

  mail to: toMail, cc: 'guillermopolito@gmail.com', subject: "[Pharo ${buildKind}] Build #${env.BUILD_NUMBER}: ${title}", body: body
  } catch (e) {
    //If there is an error during mail send, just print it and continue
    echo 'Error while sending email: ' + e.toString()
  } finally {
    cleanWs()
  }}}
}



def defineIsoTestStage(projectName, testPackages=""){
    stage("Tests-ISO-" + projectName) {
        def testGroup = "Tests"
        timeout(2) {
            dir(env.STAGE_NAME){
                def PHARO_MAJOR = shellOutput('git describe --tags --first-parent | cut -d\'-\' -f 1 | cut -c 2- | cut -d\'.\' -f 1-1')
                def PHARO_MINOR = shellOutput('git describe --tags --first-parent | cut -d\'-\' -f 1 | cut -c 2- | cut -d\'.\' -f 2-2')
                def PHARO_SHORT = PHARO_MAJOR + PHARO_MINOR

                unstash "bootstrap64"
                unzip "build/bootstrap-cache/metacello.zip"
                shell "bash -c './bootstrap/scripts/getPharoVM.sh ${PHARO_SHORT}'"
                shell "bash -c './pharo metacello.image metacello install --save --strict --signalErrorOnWarning \"filetree://../src\" SUnit --groups Core'"
                shell "bash -c './pharo metacello.image metacello install --save --strict --signalErrorOnWarning \"filetree://../src\" " + projectName + " --groups " + testGroup + "'"
                /*
                Some Baselines do specify tests in the Tests group that do not run on isolation.
                For that scenario, users can define an explicit list of packages as `testPackages`.
                In that case, take the packages specified by the user instead of the project packages.
                */
                def testPackageArguments = testPackages == "" ? "--project-name ${projectName}" : testPackages
                shell "bash -c './pharo metacello.image test --junit-xml-output --stage-name ${env.STAGE_NAME} ${testPackageArguments}'"
                junit allowEmptyResults: false, testResults: "${env.STAGE_NAME}*.xml"
            }
        }
    }
}

def bootstrapImage(){
    cleanWs()
    try {
        stage ("Fetch Requirements") {
            checkout scm
            // Stage 1 is to remove any artefacts, not required for Jenkins
            shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=64 bash ./bootstrap/scripts/2-download.sh"
        }

        stage ("Bootstrap") {
            shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=64 bash ./bootstrap/scripts/3-prepare.sh"
        }

        stage ("Metacello") {
            shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=64 bash ./bootstrap/scripts/4-installMetacello.sh"
            stash includes: "build/bootstrap-cache/*.zip,build/bootstrap-cache/*.sources,bootstrap/scripts/**,tests/**", name: "bootstrap64"
        }

        def isoTesters = [:]
        isoTesters['SUnit'] = { defineIsoTestStage("SUnit") }
        isoTesters['Kernel'] = { defineIsoTestStage("Kernel", "\'Kernel-Tests\'  \'Kernel-CodeModel-Tests\'") }
        isoTesters['Compiler'] = { defineIsoTestStage("Compiler", "\'OpalCompiler-Tests\'  \'DebugInfo-Tests\' \'Kernel-Extended-Tests\' \'Kernel-Tests-WithCompiler\'") }
        isoTesters['Files'] = { defineIsoTestStage("Files") }
        isoTesters['Zinc-Character-Encoding'] = { defineIsoTestStage("ZincCharacterEncoding") }
        isoTesters['System-SessionManager'] = { defineIsoTestStage("SystemSessionManager") }
        isoTesters['System-Platforms'] = { defineIsoTestStage("SystemPlatforms") }
        isoTesters['Announcements-Core'] = { defineIsoTestStage("Announcements") }
        isoTesters['Shift-ClassBuilder'] = { defineIsoTestStage("Shift") }
        isoTesters['System-CommandLineHandler'] = { defineIsoTestStage("SystemCommandLineHandler") }
        isoTesters['FileSystem'] = { defineIsoTestStage("FileSystem") }
        isoTesters['System-Finalization'] = { defineIsoTestStage("SystemFinalization") }
        isoTesters['System-Support'] = { defineIsoTestStage("SystemSupport") }
        isoTesters['System-Version'] = { defineIsoTestStage("SystemVersion") }
        isoTesters['Collections'] = { defineIsoTestStage("Collections") }
        isoTesters['Refactoring'] = { defineIsoTestStage("Refactoring") }
        isoTesters['DebuggerCLI'] = { defineIsoTestStage("DebuggerCLI") }
        isoTesters['Clap'] = { defineIsoTestStage("Clap") }
        parallel isoTesters

        stage ("Full Image") {
            shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=64 bash ./bootstrap/scripts/5-installIDE.sh"
            stash includes: "build/bootstrap-cache/*.zip,build/bootstrap-cache/*.sources,bootstrap/scripts/**,tests/**", name: "bootstrap64"
        }

        if( isDevelopmentBranch() ) {
            stage("Upload to files.pharo.org") {
                dir("build/bootstrap-cache") {
                    shell "BUILD_NUMBER=${env.BUILD_ID} bash ../../bootstrap/scripts/prepare_for_upload.sh 64"
                    sshagent (credentials: ['files-pharo-org-inria']) {
                        shell "bash ../../bootstrap/scripts/upload_to_files.pharo.org.sh"
                    }
                }
            }
        }
    } finally {
        shell "ls -la"
        if(fileExists('PharoDebug.log')){
            shell "mv PharoDebug.log PharoDebug-bootstrap.log"
            archiveArtifacts allowEmptyArchive: true, artifacts: "PharoDebug-bootstrap.log", fingerprint: true
        }
        if(fileExists('crash.dmp')){
            shell "mv crash.dmp crash-bootstrap.dmp"
            archiveArtifacts allowEmptyArchive: true, artifacts: "crash-bootstrap.dmp", fingerprint: true
        }

        archiveArtifacts artifacts: 'build/bootstrap-cache/*.zip,build/bootstrap-cache/*.sources', fingerprint: true
        cleanWs()
    }
}

def launchBenchmark(){
    node('unix'){
		stage('launchBenchmark'){
		  cleanWs()

			projectName = env.JOB_NAME
		  //We checkout scm to have access to the log information
		  checkout scm

		  if (env.CHANGE_ID != null) {
				//If I am in a PR the head of the repository is a merge of the base commit (the development branch) and the PR commit.
				//I take the first parent of this commit. It is the commit in the PR
				commit = shellOutput('git log HEAD^1 -1 --format="%H"')
				isPR = true
			} else {
				// If it is not a PR the commit to evaluate and put the status in github is the current commit
				commit = shellOutput('git log -1 --format="%H"')
				isPR = false
			}

			build job: 'pharo-benchmarks', parameters: [text(name: 'originProjectName', value: projectName), booleanParam(name: 'isPR', value: isPR), text(name: 'commit', value: commit)], wait: false
		}
	}
}

try{
  properties([disableConcurrentBuilds()])

  // We run the whole process in 64 bits all the time now
  def architectures = ['64']

  node('unix') {
    timeout(60) {
      bootstrapImage()
    }
  }

  //Testing step
  def testers = [:]

  def platforms = ['unix', 'osx', 'windows']
  for (arch in architectures) {
    // Need to bind the label variable before the closure - can't do 'for (label in labels)'
    def architecture = arch
    for (platf in platforms) {
      // Need to bind the label variable before the closure - can't do 'for (label in labels)'
      def platform = platf
      // Disabling the test of 32bits
      if (arch != '32') {
        testers["${platform}-${architecture}"] = {
          node(platform) {
            stage("Tests-${platform}-${architecture}") {
              timeout(35) {
                runTests(architecture)
              }
            }
          }
        }
      }
    }
  }

  testers["unix-commandline"] = {
    node("unix") {
      stage("Tests-Pharo-command-line") {
        timeout(5) {
          runCommandLineTests()
        }
      }
    }
  }

  parallel testers

  notifyBuild("SUCCESS")

  launchBenchmark()
} catch (e) {
  notifyBuild("FAILURE")
  throw e
}
