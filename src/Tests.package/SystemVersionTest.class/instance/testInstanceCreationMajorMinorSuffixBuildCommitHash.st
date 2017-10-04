tests - instance creation
testInstanceCreationMajorMinorSuffixBuildCommitHash

	| version |
	version := (SystemVersion major: 7 minor: 0 suffix: 'alpha' build: 148 commitHash: '368dcda1b1ba81215fb7355cfc1e901d45395bd8').
	self 
		assert: version type equals: 'Pharo';	
		assert: version major equals: 7;
		assert: version minor equals: 0;
		assert: version suffix equals: 'alpha';
		assert: version build equals: 148;
		assert: version commitHash equals: '368dcda1b1ba81215fb7355cfc1e901d45395bd8';
		assert: version commitHashShort equals: '368dcda';
		assert: version date notNil; 	
		assert: (version printString beginsWith: 'Pharo-7.0+alpha.build.148.sha.368dcda1b1ba81215fb7355cfc1e901d45395bd8')