tests
testSelectedMethod

	self assert: nautilus selectedMethod isNil.
	nautilus selectedMethod: (Object>>#at:).
	self assert: (nautilus selectedMethod = (Object>>#at:))