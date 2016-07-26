tests-default state
testTextShouldBeEmptyByDefault

	self assert: text isEmpty.
	self assert: text asString equals: String empty.