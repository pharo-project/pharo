private
ruleFor: aTestSelector plusSelectors: aCollectionOfTestedSelectors
	| rule expectedResultEnvironment |
	rule := self
		runRuleFor: aTestSelector
		onEnvironment: (self classEnvironmentWith: RBSmalllintTestObject).
	expectedResultEnvironment := RBSelectorEnvironment new.
	(aCollectionOfTestedSelectors
		copyWith: (self objectClassSelectorFor: aTestSelector))
		do: [ :each | 
			expectedResultEnvironment addClass: RBSmalllintTestObject selector: each.
			self
				assert: (RBSmalllintTestObject includesSelector: each)
				description: 'Missing test method RBSmalllintTestObject>>' , each printString ].
	self
		assertResultOfRule: rule
		hasSameSelectorsAsEnvironment: expectedResultEnvironment