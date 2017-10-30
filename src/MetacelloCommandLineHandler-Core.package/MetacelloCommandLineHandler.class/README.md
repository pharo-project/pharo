Command line handler for dealing with Metacello from the command line.

Usage:
metacello --help
metacello install <repository url> (<baseline>|<configuration>) [--version=<version>] [--groups=<group name>,...] [--no-quit] [--no-save]
	<repository url>    A Monticello repository name 
	<baseline>          A valid Metacello Baseline name
	<configuration>     A valid Metacello Configuration name
	<version>           A valid version for the given Configuration (incompatible with Baseline)
	<group name>        A valid Metacello group name(s)
	--help              Show this help message
	--no-quit           Keep the image running after Baseline/Configuration install
	--no-save           Don't save the image after Baseline/Configuration install
	
Examples:
	# Display this help message
	pharo Pharo.image metacello OR pharo Pharo.image metacello --help
	
	# Install group 'Core' and 'Tests' of latest version
	pharo Pharo.image metacello install $REPOS_URL BaselineOfFoo --groups=Core,Tests
	
	# Install a specific version '1.5' and only a specific group 'Tests'
	pharo Pharo.image metacello install $REPOS_URL ConfigurationOfFoo --version=1.5 --groups=Tests
	
	