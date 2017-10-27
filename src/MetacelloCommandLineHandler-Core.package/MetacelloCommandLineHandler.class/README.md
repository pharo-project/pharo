Command line handler for dealing with Metacello Baselines/Configurations from the command line

Usage:
metacello --help
metacello install <repository url> (<baseline>|<configuration>) [--version=<version>] [--groups=<group name>,...] [--no-quit] [--no-save]
	--help              Show this help message
	--no-quit        Keep the image running after Baseline/Configuration install
	--no-save       Don't save the image after Baseline/Configuration install
	<repository url>    A Monticello repository name 
	<baseline>     A valid Metacello Baseline name
	<configuration>     A valid Metacello Configuration name
	<version>           A valid version for the given Baseline/Configuration
	<group name>            A valid Metacello group name(s)
	
Examples:
	TODO