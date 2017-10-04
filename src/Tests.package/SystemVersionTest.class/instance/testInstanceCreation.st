tests - instance creation
testInstanceCreation

	| version |
	version := (SystemVersion new).
	self 
		assert: version type equals: 'Pharo';
		assert: version major equals: 0;
		assert: version minor equals: 0;
		assert: version suffix isEmpty;
		assert: version build equals: SystemVersion invalidBuildNumber;	
		assert: version commitHash isEmpty;
		assert: version commitHashShort isEmpty;
		assert: version commitHashShort isEmpty;		
		assert: version date notNil;
		assert: (version printString beginsWith: 'Pharo-0.0')