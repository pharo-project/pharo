testing-method
testMethodSelector
	| pragma |
	pragma := self pragma: 'foo' selector: #bar.
	self assert: pragma methodSelector == #bar.