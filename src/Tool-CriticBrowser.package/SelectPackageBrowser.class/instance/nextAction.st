api
nextAction

	| env packages |	
		
	packages := selectedModel listItems.
	env := RBPackageEnvironment new packages: packages.
	SelectRuleBrowser openWithEnvironment: env removeTestCase: self addTestCaseClasses state.
	self delete
	
		
