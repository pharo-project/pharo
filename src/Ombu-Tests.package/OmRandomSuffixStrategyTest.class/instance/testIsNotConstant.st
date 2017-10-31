tests
testIsNotConstant

	| names |
	names := (1 to: 7) collect: [:each | strategy nextTo: 'existing' in: self directory ].

	"This should be different"
	self assert: names asSet size equals: names size.