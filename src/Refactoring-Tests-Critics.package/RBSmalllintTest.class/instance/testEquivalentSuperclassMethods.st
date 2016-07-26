tests
testEquivalentSuperclassMethods
	| returnedEnvironment rule |
	RBSmalllintChecker 
		runRule: (rule := RBEquivalentSuperclassMethodsRule new)
		onEnvironment: self smalllintTestEnvironment.
	returnedEnvironment := RBSelectorEnvironment new.
	returnedEnvironment 
		addClass: RBSmalllintTestObject
		selector: #isLiteral.
	self 
		checkRule: rule
		isEqualTo: returnedEnvironment