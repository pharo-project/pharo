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

def runTests(architecture, prefix=''){
  cleanWs()
  dir(env.STAGE_NAME) {
    try {
        unstash "bootstrap${architecture}"
        shell "bash -c 'bootstrap/scripts/run${prefix}Tests.sh ${architecture} ${env.STAGE_NAME}'"
        junit allowEmptyResults: true, testResults: "${env.STAGE_NAME}*.xml"
        archiveArtifacts allowEmptyArchive: true, artifacts: "${env.STAGE_NAME}*.xml", fingerprint: true
    }finally{
        // I am archiving the logs to check for crashes and errors.
        if(fileExists('PharoDebug.log')){
            shell "mv PharoDebug.log PharoDebug-${env.STAGE_NAME}.log"
            archiveArtifacts allowEmptyArchive: true, artifacts: "PharoDebug-${env.STAGE_NAME}.log", fingerprint: true
        }
        if(fileExists('crash.dmp')){
            shell "mv crash.dmp crash-${env.STAGE_NAME}.dmp"
            archiveArtifacts allowEmptyArchive: true, artifacts: "crash-${env.STAGE_NAME}.dmp", fingerprint: true
        }
    }
  }
}

def shellOutput(params){
    return (isWindows())? bat(returnStdout: true, script: params).trim() : sh(returnStdout: true, script: params).trim()
}

def notifyBuild(status){
  node('unix'){ stage('notify'){
  try{
  
  //If this is development, we send the email to the bugtracker list
  //Otherwise, we send it to pharo-dev
  def toMail = "pharo-bugtracker@lists.gforge.inria.fr"
  def buildKind = env.BRANCH_NAME
  if (env.CHANGE_ID != null){
    buildKind = "PR ${env.CHANGE_ID}"
  }
  if( isDevelopmentBranch() ) {
    toMail = "pharo-dev@lists.pharo.org"
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
      def fogbugzUrl = "https://pharo.fogbugz.com/f/cases/${issueNumber}"
      
      mailMessage += """
Issue Url: ${fogbugzUrl}"""
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

def bootstrapImage(){
   cleanWs()
  def builders = [:]
  def architectures = ['32']//, '64']
  for (arch in architectures) {
      // Need to bind the label variable before the closure - can't do 'for (label in labels)'
      def architecture = arch

      builders[architecture] = {
        dir(architecture) {

        try{
          stage ("Fetch Requirements-${architecture}") {  
            checkout scm
            // Stage 1 is to remove any artefacts, not required for Jenkins
            shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=${architecture} bash ./bootstrap/scripts/2-download.sh"
          }

        stage ("Bootstrap-${architecture}") {
      shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=${architecture} bash ./bootstrap/scripts/3-prepare.sh"
            }

    stage ("Full Image-${architecture}") {
      shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=${architecture} bash ./bootstrap/scripts/4-build.sh"
      stash includes: "bootstrap-cache/*.zip,bootstrap-cache/*.sources,bootstrap/scripts/**", name: "bootstrap${architecture}"
    }
  
        if (architecture == "32") {
        stage ("Convert Image - 32->64") {
          dir("conversion") {
            shell "cp ../bootstrap-cache/*.zip ."
            shell "bash ../bootstrap/scripts/transform_32_into_64.sh"
            shell "mv *-64bit-*.zip ../bootstrap-cache"
          }
        }
      }
  
      if( isDevelopmentBranch() ) {
        stage("Upload to files.pharo.org") {
          dir("bootstrap-cache") {
              shell "BUILD_NUMBER=${env.BUILD_ID} bash ../bootstrap/scripts/prepare_for_upload.sh"
            sshagent (credentials: ['b5248b59-a193-4457-8459-e28e9eb29ed7']) {
              shell "bash ../bootstrap/scripts/upload_to_files.pharo.org.sh"
            }
          }
        }
      }

      } finally {
        archiveArtifacts artifacts: 'bootstrap-cache/*.zip,bootstrap-cache/*.sources', fingerprint: true
        cleanWs()
      }
      }
    }
  
  }
  parallel builders 
}

try{
    properties([disableConcurrentBuilds()])
  
    node('unix') {
      timeout(30) {
        bootstrapImage()
      }
    }
    

    //Testing step
    def testers = [:]
    def architectures = ['32']//, '64']
    def platforms = ['unix', 'osx', 'windows']
    for (arch in architectures) {
      // Need to bind the label variable before the closure - can't do 'for (label in labels)'
      def architecture = arch
      for (platf in platforms) {
        // Need to bind the label variable before the closure - can't do 'for (label in labels)'
        def platform = platf
        testers["${platform}-${architecture}"] = {
          node(platform) { stage("Tests-${platform}-${architecture}") {
            timeout(35) {
              runTests(architecture)
              runTests(architecture, "Kernel")
            }
          }}
        }
      }
    }
    parallel testers

  notifyBuild("SUCCESS")
} catch (e) {
  notifyBuild("FAILURE")
  throw e
}
