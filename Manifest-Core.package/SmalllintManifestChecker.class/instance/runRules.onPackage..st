actions
runRules: aCompositeRule onPackage: aPackage

	rule := aCompositeRule.
	environment := RBPackageEnvironment package: aPackage.
	self run.