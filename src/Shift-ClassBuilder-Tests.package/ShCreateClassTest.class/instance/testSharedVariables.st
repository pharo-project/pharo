tests
testSharedVariables
	builder name: #SHClassWithSharedVariable.
	result := builder
		sharedVariables: {(#aSharedVariable => ClassVariable)};
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateSharedVariables: #(aSharedVariable)