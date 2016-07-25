tests - converting
testAsSetWithEqualsElements
	| t1 |
	t1 := self withEqualElements asSet.
	self withEqualElements
		do: [:t2 | self assert: (t1 occurrencesOf: t2)
					= 1].
	self assert: t1 class = IdentitySet