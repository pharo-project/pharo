tests
testForcedRuleOnClassWithGlobalBan
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	ReImageRuleBaner ban: criticToSkip.
	
	testPackage packageManifest class compile:
		'forcedRules ^ #(',
		ScreamerRule name,
		')'.
		
	ReRuleManager reset.
	
	self assert: (testClass critiques anySatisfy: [ :critic |
		critic rule class = ScreamerRule ]).