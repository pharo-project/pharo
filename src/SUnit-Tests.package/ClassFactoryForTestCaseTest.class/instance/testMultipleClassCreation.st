testing
testMultipleClassCreation
	5 timesRepeat: [
		factory newClass].
	self assert: (SystemNavigation new allClasses includesAll: factory createdClasses).
	self assert: factory createdClassNames asSet size = 5.
	self assert: (SystemOrganization listAtCategoryNamed: factory defaultCategory) asSet = factory createdClassNames asSet