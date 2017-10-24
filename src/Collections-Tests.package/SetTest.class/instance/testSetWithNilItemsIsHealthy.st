tests - integrity
testSetWithNilItemsIsHealthy
	self classToBeTested = Set
		ifFalse: [ ^ self	"only works on Dictionary" ].
	self assert: {nil} asSet isHealthy