tests
testReverseUnevenDo
	| s i |
	s := OrderedCollection new.
	i := 10 to: 20 by: 3.
	i
		reverseDo: [:each | s addFirst: each].
	self
		assert: (s hasEqualElements: i)