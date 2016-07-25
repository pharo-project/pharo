tests
testShouldntRaiseError
	self
		ruleFor: self currentSelector
		plusSelectors: #(shouldntRaiseException)