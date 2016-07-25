testing
testOverride
	| selectors |
	self assert: model notEmpty.
	self assert: model entries size = 3.
	selectors := model entriesOfType: #selector.
	self
		assert: (selectors includes: #toBeOverriden:).
	self
		assert: (selectors includes: #initialize).
	self
		assert: (selectors includes: #toBeOverridenWithReturn)