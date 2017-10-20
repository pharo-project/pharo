tests - instance creation
testInstanceCreationMajorMinorSuffixBuildWithoutSuffix

	| version |
	version := (SystemVersion major: 7 minor: 0 suffix: '' build: 142).
	self 
		assert: version type equals: 'Pharo';	
		assert: version major equals: 7;
		assert: version minor equals: 0;
		assert: version suffix isEmpty;
		assert: version build equals: 142;
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;
		assert: version date notNil;		
		assert: (version printString beginsWith: 'Pharo-7.0+build.142')