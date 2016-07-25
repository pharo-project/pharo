tests-basic
testNewVariableByteClass

	aClass := self makeWithLayout: ByteLayout.
	self deny: aClass isPointers.
	self deny: aClass isFixed.
	self assert: aClass isBytes.
	self assert: aClass isVariable.