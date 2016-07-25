actions
runRules: aCompositeRule onPackage: aPackage withoutTestCase: avoidTestCases
	rule := aCompositeRule.
	environment := RBPackageEnvironment package: aPackage.
	
	environment := environment withoutClasses: (environment classes select: #isManifest).
	avoidTestCases 
		ifTrue: [ environment := environment withoutClasses: (environment classes select: #isTestCase) ].
	self run