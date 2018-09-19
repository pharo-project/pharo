instance creation
fromCommandLine
	| options architecture buildNumber repo versionInfo |
	options := CommandLineArguments new.
	architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].
	buildNumber := options optionAt: 'BUILD_NUMBER' ifAbsent: [ '1' ].
	versionInfo := options optionAt: 'VERSION_INFO' ifAbsent: [ self error: 'Must provide VERSION_INFO in the form of a git describe --long' ].
	
	('Architecture: ' , architecture) logCr.
	('BuildNumber: ' , buildNumber) logCr.
	('VersionInfo: ' , versionInfo) logCr. 
	
	versionInfo := (PBVersionInfo fromLongGitDescribe: versionInfo)
		buildNumber: buildNumber;
		yourself.
	
	^ super new
		architecture: architecture;
		versionInfo: versionInfo;
		imageName: 'bootstrap.image';
		yourself