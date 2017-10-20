tests
testForcedRuleOnMethodWithPackageBan
	| criticToSkip |
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testPackage ban: criticToSkip.
	
	testPackage packageManifest class compile:
		'forcedRules ^ #(',
		ScreamerRule name,
		')'.
		
	ReRuleManager reset.
	
	self assert: (testMethod critiques anySatisfy: [ :critic |
		critic rule class = ScreamerRule ]).