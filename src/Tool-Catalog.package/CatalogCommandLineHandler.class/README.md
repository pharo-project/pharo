Command line handler for dealing with projects from the command line

Usage: get [--help] ProjectName [--version=<version>]
	--help                        Show this help message
	ProjectName         The name of the project in catalog
	<version>                A valid version for the given configuration (default is #stable)
	
Examples:
	# installs Seaside3 
	pharo Pharo.image get Seaside3
	
	# installs Seaside3 version 3.1.2
	pharo Pharo.image get Seaside3 --version=3.1.2