torevisit- tests - accessing
testCollectCreatesANewGrid
	| m m2 |
	m := self gridClass new: 3.
	m atAllPut: -1.
	self assert: (m occurrencesOf: -1) equals: 9.
	m2 := m collect: [ :each | each abs ].
	m2 collect: [ :each | self assert: each equals: 1 ].
	self assert: (m2 occurrencesOf: 1) equals: 9