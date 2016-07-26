tests
testIsUsed
	self assert: Object isUsed.
	self assert: Object class isUsed.
	self assert: TBehavior isUsed.
	self assert: TBehavior classTrait isUsed.
