def isWindows(){
    return env.NODE_LABELS.toLowerCase().contains('windows')
}

def shell(params){
    if(isWindows()) bat(params) 
    else sh(params)
}

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
			shell "./pharo Pharo.image ./bootstrap/scripts/generateHermesFiles.st --quit"
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
				try {
					cleanWs()
					unstash "bootstrap${architecture}"
					shell "bash -c 'bootstrap/scripts/runTests.sh ${architecture}'"
				} finally {
					archiveArtifacts allowEmptyArchive: true, artifacts: '*.xml', fingerprint: true
					junit allowEmptyResults: true, testResults: '*.xml'
					cleanWs()
				}
			}}
		}
	}
}
parallel testers