tests
testSelectedClass

	self assert: nautilus selectedMethod isNil.
	nautilus selectedMethod: (Object>>#at:).
	nautilus selectedCategory: 'test'.
	self assert: (nautilus selectedCategory = 'test').
	self assert: (nautilus selectedMethod isNil).
	
	nautilus selectedClass: Morph.
	self assert: (nautilus selectedClass = Morph).
	self assert: (nautilus selectedCategory isNil).
	self assert: (nautilus selectedMethod isNil).