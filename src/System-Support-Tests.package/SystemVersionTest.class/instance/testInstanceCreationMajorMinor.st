tests - instance creation
testInstanceCreationMajorMinor

	| version |
	version := (SystemVersion major: 6 minor: 1).
	self 
		assert: version type equals: 'Pharo';
		assert: version major equals: 6;
		assert: version minor equals: 1;
		assert: version suffix isEmpty;
		assert: version build equals: SystemVersion invalidBuildNumber;
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;		
		assert: version date notNil;
		assert: (version printString beginsWith: 'Pharo-6.1')