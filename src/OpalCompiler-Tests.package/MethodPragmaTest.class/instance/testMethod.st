testing-method
testMethod
	| pragma |
	pragma := self pragma: 'foo' selector: #bar.
	self assert: pragma method == (self class >> #bar).