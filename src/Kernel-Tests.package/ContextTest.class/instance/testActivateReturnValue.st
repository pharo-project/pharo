tests
testActivateReturnValue
	self assert:  ((aSender activateReturn: aMethodContext value: #()) isKindOf: Context).
	self assert:  ((aSender activateReturn: aMethodContext value: #()) receiver = aMethodContext).