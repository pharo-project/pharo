tests
testSelectedCategory

	self assert: nautilus selectedMethod isNil.
	nautilus selectedMethod: (Object>>#at:).
	nautilus selectedCategory: 'test'.
	self assert: (nautilus selectedCategory = 'test').
	self assert: (nautilus selectedMethod isNil).