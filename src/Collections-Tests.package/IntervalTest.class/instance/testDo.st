tests
testDo
	| s i |
	s := OrderedCollection new.
	i := (10 to: 20).
	i do: [ :each | s addLast: each].
	self assert: (s hasEqualElements: i)