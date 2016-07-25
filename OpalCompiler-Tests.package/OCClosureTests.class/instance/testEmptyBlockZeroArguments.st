testing-empty
testEmptyBlockZeroArguments
	self
		assert: (self class compiler evaluate: '[ ] value') isNil
		description: 'Empty blocks in ST-80 should return nil'.
	self
		assert: (self class compiler evaluate: '[ | t | ] value') isNil
		description: 'Empty blocks in ST-80 should return nil'