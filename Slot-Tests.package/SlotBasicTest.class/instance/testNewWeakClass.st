tests-basic
testNewWeakClass

	aClass := self makeWithLayout: WeakLayout.
	self assert: aClass isPointers.
	self deny: aClass isFixed.
	self assert: aClass isWords.
	self assert: aClass isWeak.