initialization
setUp
	super setUp.

	brokenRule := ReAbstractRule
		subclass: #ReBrokenRuleForTesting
		instanceVariableNames: ''
		classVariableNames: ''
		package: RPackage defaultPackageName.
		
	brokenRule class
		compile: 'checksMethod ^ true'.
		
	brokenRule
		compile: 'name ^ ''A purpously broken rule to test exception handling''';
		compile: 'check: aNode forCritiquesDo: aBlock
						MyTestError signal: ',
						self errorMessage surroundedBySingleQuotes.
						
	ReRuleManager reset