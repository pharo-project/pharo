testing-method
testMethodClass
	| pragma |
	pragma := self pragma: 'foo' selector: #bar.
	self assert: pragma methodClass == self class.