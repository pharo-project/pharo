running
setupScreamerRule

	ScreamerRule := ReAbstractRule
		subclass: #ReScreamerRuleForTesting
		instanceVariableNames: ''
		classVariableNames: ''
		package: testPackage name.
		
	ScreamerRule class
		compile: 'checksMethod ^ true';
		compile: 'checksClass ^ true';
		compile: 'checksPackage ^ true'.
		
	ScreamerRule
		compile: 'name ^ ''A tautological violation for testing''';
		compile: 'check: aNode forCritiquesDo: aBlock
						aBlock cull: (self critiqueFor: aNode)'.
						
	ReRuleManager reset