tests-default state
testSelectionShouldBeEmptyByDefault

	self assert: text isEmpty.
	self assert: selection isEmpty
	