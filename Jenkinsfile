def shell(command) {
	try{
		sh "${command} > output.txt"
		return readFile("output.txt").trim()
	}catch(e){
	        echo readFile("output.txt").trim()
		throw e
	}
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
			shell "./pharo ./Pharo.image bootstrap/scripts/bootstrap.st --ARCH=${architecture} --quit"
	    }

		stage ("Full Image-${architecture}") {
			shell "BOOTSTRAP_ARCH=${architecture} bash ./bootstrap/scripts/build.sh -a ${architecture}"
			stash includes: "bootstrap-cache/*.zip,bootstrap-cache/*.sources", name: "bootstrap${architecture}"
	    }
		} finally {
			archiveArtifacts artifacts: 'bootstrap-cache/*.zip,bootstrap-cache/*.sources', fingerprint: true
		}
		
		// platforms for Jenkins node types we will build on
		def platforms = ['unix', 'osx', 'windows']
		def testers = [:]
		for (platf in platforms) {
	        // Need to bind the label variable before the closure - can't do 'for (label in labels)'
	        def platform = platf
		    testers["${platform}-${architecture}"] = {
	            node(platform) { stage("Tests-${platform}-${architecture}"){
					try {
					cleanWs()
		            unstash "bootstrap${architecture}"
					
					def urlprefix = ""
					if (architecture == "64" ) {
						urlprefix = "/64"
					}
					
					def imageArchive = shell "find bootstrap-cache -name 'Pharo7.0-${architecture}bit-*.zip'"
					shell "unzip ${imageArchive}"
					def imageFile= shell "find . -name 'Pharo7.0-${architecture}bit-*.image'"
					def changesFile= shell "find . -name 'Pharo7.0-${architecture}bit-*.changes'"
					
					shell "cp bootstrap-cache/*.sources ."
					shell "mv ${imageFile} Pharo.image"
					shell "mv ${changesFile} Pharo.changes"
					
					shell "wget -O - get.pharo.org${urlprefix}/vm70 | bash"
					shell "./pharo Pharo.image test --junit-xml-output \".*\""
					} finally {
						archiveArtifacts allowEmptyArchive: true, artifacts: '*.xml', fingerprint: true
						junit allowEmptyResults: true, testResults: '*.xml'
					}
				}}
		    }
		}
		parallel testers
		
		}
	} // end build block
	
	} // end for architectures
	
	parallel builders
	cleanWs()
	
} // end node
