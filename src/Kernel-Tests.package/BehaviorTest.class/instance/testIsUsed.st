tests
testIsUsed
	self assert: Object isUsed.
	self assert: Object class isUsed.
	self assert: Trait1 isUsed.
	self assert: Trait1 classTrait isUsed.
