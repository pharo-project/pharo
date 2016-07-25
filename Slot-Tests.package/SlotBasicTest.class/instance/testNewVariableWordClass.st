tests-basic
testNewVariableWordClass

	aClass := self makeWithLayout: WordLayout.
	self deny: aClass isPointers.
	self deny: aClass isFixed.
	self assert: aClass isWords.
	self assert: aClass isVariable.