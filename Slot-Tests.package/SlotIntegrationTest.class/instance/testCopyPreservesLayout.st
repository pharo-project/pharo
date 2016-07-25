tests
testCopyPreservesLayout
	aClass := self makeWithLayout: FixedLayout.
	anotherClass := aClass copy.
	self deny: anotherClass classLayout isNil.
	self deny: anotherClass classLayout == aClass classLayout.
	self assert: aClass classLayout host == aClass.
	self assert: anotherClass classLayout host == anotherClass