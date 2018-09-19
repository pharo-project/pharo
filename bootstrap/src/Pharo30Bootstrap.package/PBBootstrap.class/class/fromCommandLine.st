instance creation
fromCommandLine
	| options architecture buildNumber repo versionInfo |
	options := CommandLineArguments default.
	architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].
	buildNumber := options optionAt: 'BUILD_NUMBER' ifAbsent: [ '1' ].
	
	('Architecture: ' , architecture) logCr.
	('BuildNumber: ' , buildNumber) logCr. 
	
	repo := IceRepositoryCreator new
		location: self repositoryLocation;
		subdirectory: 'src';
		createRepository.
	
	versionInfo := (PBVersionInfo major: 7)
		repository: repo;
		buildNumber: buildNumber;
		minor: 0;
		yourself.
	
	^ super new
		architecture: architecture;
		versionInfo: versionInfo;
		imageName: 'bootstrap.image';
		yourself