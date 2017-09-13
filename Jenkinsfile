import hudson.tasks.test.AbstractTestResultAction

def isWindows(){
	//If NODE_LABELS environment variable is null, we assume we are on master unix machine
	if (env.NODE_LABELS == null) {
		return false
	}
    return env.NODE_LABELS.toLowerCase().contains('windows')
}

def shell(params){
    if(isWindows()) bat(params) 
    else sh(params)
}

def runTests(architecture, prefix=''){
	def retryTimes = 3
	def tries = 0
	def success = false
	waitUntil {
		tries += 1
		echo "Try #${tries}"
		try {
			cleanWs()
			unstash "bootstrap${architecture}"
			shell "bash -c 'bootstrap/scripts/run${prefix}Tests.sh ${architecture}'"
			junit allowEmptyResults: true, testResults: '*.xml'
			success = !(currentBuild.result == 'UNSTABLE')
			echo "Tests run with result ${currentBuild.result}"
		} catch(e) {
			//If there is an exception ignore.
			//success will be false and we will retry thanks to waitUntil
			currentBuild.result == 'FAILURE'
			echo "Tests couldn't complete to run due to an exception"
		}
		if (!success && tries == retryTimes) {
			echo "Out of retries"
		}
		return success || (tries == retryTimes)
	}
	archiveArtifacts allowEmptyArchive: true, artifacts: '*.xml', fingerprint: true
	cleanWs()
}

def shellOutput(params){
    return (isWindows())? bat(returnStdout: true, script: params).trim() : sh(returnStdout: true, script: params).trim()
}

def notifyBuild(status){
	try{
	if( env.BRANCH_NAME != "development" ) {
		//Should only notify in development
		return
	}
	
	def owner = "pharo-project"
	def title = status
	
	//Get the merge information from the last commit
	def logMessage = shellOutput('git log -1 --format="%B"')
	def logSHA = shellOutput('git log -1 --format="%p"')
	
	def mailMessage = "Could not extract further issue information from commit message: ${logMessage}"
	
	//If there is no pull request information, we will send a log with the last commit message only
	def isPRMergeCommit = logMessage ==~ "Merge pull request #[0-9]+ from.*"	
	if (isPRMergeCommit) {
		def pullRequestId = logMessage.split(' ')[3].substring(1)
		def githubPullRequestHttpRequest = "https://api.github.com/repos/${owner}/pharo/pulls/${pullRequestId}"
		def response = httpRequest githubPullRequestHttpRequest
		if (response.status == 200) { 
			def pullRequestJSON = readJSON text: response.content
			def pullRequestTitle = pullRequestJSON['title']
			
			def pullRequestUrl = "https://github.com/pharo-project/${owner}/pulls/${pullRequestId}"
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

Check for latest built images in http://files.pharo.org:
 - http://files.pharo.org/images/70/Pharo-7.0.0-alpha.build.${env.BUILD_NUMBER}.sha.${logSHA}.arch.32bit.zip
 - http://files.pharo.org/images/70/Pharo-7.0.0-alpha.build.${env.BUILD_NUMBER}.sha.${logSHA}.arch.64bit.zip
"""
	mail to: 'pharo-dev@lists.pharo.org', cc: 'guillermopolito@gmail.com', subject: "[Pharo 7.0-dev] Build #${env.BUILD_NUMBER}: ${title}", body: body
	} catch (e) {
		//If there is an error during mail send, just print it and continue
		echo 'Error while sending email: ' + e.toString()
	}
}

try{

node('unix') {
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
			shell 'wget -O - get.pharo.org/vm60 | bash	'
			shell 'wget https://github.com/guillep/PharoBootstrap/releases/download/v1.1.1/bootstrapImage.zip'
			shell 'unzip bootstrapImage.zip'
			shell './pharo Pharo.image bootstrap/scripts/prepare_image.st --save --quit'
	    }

		stage ("Bootstrap-${architecture}") {
			shell "mkdir -p bootstrap-cache #required to generate hermes files"
			shell "./pharo Pharo.image ./bootstrap/scripts/generateKernelHermesFiles.st --quit"
			shell "./pharo Pharo.image ./bootstrap/scripts/generateSUnitHermesFiles.st --quit"
			shell "./pharo ./Pharo.image bootstrap/scripts/bootstrap.st --ARCH=${architecture} --BUILD_NUMBER=${env.BUILD_ID} --quit"
	    }

		stage ("Full Image-${architecture}") {
			shell "BUILD_NUMBER=${BUILD_NUMBER} BOOTSTRAP_ARCH=${architecture} bash ./bootstrap/scripts/build.sh -a ${architecture}"
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
		
		if( env.BRANCH_NAME == "development" ) {
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
				runTests(architecture)
			}}
		}
		testers["kernel-${platform}-${architecture}"] = {
			node(platform) { stage("Kernel-tests-${platform}-${architecture}") {
				runTests(architecture, "Kernel")
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