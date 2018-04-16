instance creation
fromCommandLine
	| options architecture buildNumber |
	options := CommandLineArguments default.
	architecture := options optionAt: 'ARCH' ifAbsent: [ '32' ].
	buildNumber := options optionAt: 'BUILD_NUMBER' ifAbsent: [ '1' ].
	
	('Architecture: ' , architecture) logCr.
	('BuildNumber: ' , buildNumber) logCr. 
	
	^ super new
		architecture: architecture;
		buildNumber: buildNumber;
		imageName: 'bootstrap.image';
		yourself