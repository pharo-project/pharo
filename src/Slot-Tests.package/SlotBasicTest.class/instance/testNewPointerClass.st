tests-basic
testNewPointerClass

	aClass := self makeWithLayout: FixedLayout.
	self assert: aClass isPointers.
	self assert: aClass isFixed.
	self assert: aClass instVarNames isEmpty.