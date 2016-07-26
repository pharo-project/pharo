tests
testEquals
	| a b |
	a := self mockClass: 'ClassA' super: 'SuperA'.
	b := self mockClass: 'ClassA' super: 'SuperA'.
	self assert: a = b