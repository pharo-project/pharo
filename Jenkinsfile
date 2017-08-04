node('unix') {
	cleanWs()
	def builders = [:]
	def architectures = ['32']//, '64']
	for (arch in architectures) {
    // Need to bind the label variable before the closure - can't do 'for (label in labels)'
    def architecture = arch

	builders[architecture] = {
		dir(architecture) {
		
		stage ("Fetch Requirements-${architecture}") {	
			checkout scm
			sh 'wget -O - get.pharo.org/vm60 | bash'
			sh 'wget https://github.com/guillep/PharoBootstrap/releases/download/v1.1.1/bootstrapImage.zip'
			sh 'unzip bootstrapImage.zip'
			sh './pharo Pharo.image bootstrap/scripts/prepare_image.st --save --quit'
	    }

		stage ("Bootstrap-${architecture}") {
			sh "./pharo Pharo.image ./bootstrap/scripts/generateHermesFiles.st --quit"
			sh "./pharo ./Pharo.image bootstrap/scripts/bootstrap.st --ARCH=${architecture} --quit"
	    }

		stage ("Full Image-${architecture}") {
			sh "BOOTSTRAP_ARCH=${architecture} bash ./bootstrap/scripts/build.sh -a ${architecture}"
			stash includes: "bootstrap-cache/*.zip", name: "bootstrap${architecture}"
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
					
					def imageArchive = sh "find bootstrap-cache -name 'Pharo7.0-${ARCHITECTURE}bit-*.zip'"
					sh "unzip ${imageArchive}"
					def imageFile="find bootstrap-cache -name 'Pharo7.0-${architecture}bit-*.image'"
					def changesFile="find bootstrap-cache -name 'Pharo7.0-${architecture}bit-*.changes'"
					
					sh "cp bootstrap-cache/*.sources ."
					sh "mv ${imageFile} Pharo.image"
					sh "mv ${changesFile} Pharo.changes"
					
					sh "wget -O - get.pharo.org${urlprefix}/vm70 | bash"
					sh "./pharo Pharo.image test --junit-xml-output \".*\""
					junit "*.xml"
					} catch (Exception e) {
						println(e)
						throw e
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