Command line handler for dealing with Metacello configurations from the command line

Usage: config [--help] <repository url> [<configuration>] [--install[=<version>]] [--group=<group>] [--username=<username>] [--password=<password>][--no-quit][--no-save]
	--help              show this help message
	--no-quit        keep the image running after configuration install
	--no-save       Don't save the image after configuration install
	<repository url>    A Monticello repository name 
	<configuration>     A valid Metacello Configuration name
	<version>           A valid version for the given configuration
	<group>             A valid Metacello group name
	<username>          An optional username to access the configuration's repository
	<password>          An optional password to access the configuration's repository
	
Examples:
	# display this help message
	pharo Pharo.image config
	
	# list all configurations of a repository
	pharo Pharo.image config $MC_REPOS_URL
	
	# list all the available versions of a confgurtation
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo
	
	# install the stable version
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo --install
	
	#install a specific version '1.5'
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo --install=1.5
	
	#install a specific version '1.5' and only a specific group 'Tests'
	pharo Pharo.image config $MC_REPOS_URL ConfigurationOfFoo --install=1.5 --group=Tests
