tests
testWhitespace
	| whitespace |
	whitespace := { Character space. Character tab. Character cr. Character lf }.
	self assert: (self materialize: whitespace, '123') = 123
	