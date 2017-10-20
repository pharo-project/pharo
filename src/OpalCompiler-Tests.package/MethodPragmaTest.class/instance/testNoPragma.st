testing-compiled
testNoPragma
	| method |
	method := self compile: '' selector: #foo.
	self assert: method pragmas = #().