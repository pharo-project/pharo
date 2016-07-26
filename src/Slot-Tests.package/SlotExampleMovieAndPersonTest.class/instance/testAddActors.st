tests
testAddActors

	| m1 m2 p1 p2 |
	
	m1 := SlotExampleMovie named: 'M1'.
	m2 := SlotExampleMovie named: 'M2'.
	p1 := SlotExamplePerson named: 'P1'.
	p2 := SlotExamplePerson named: 'P2'.
	
	m1 addActor: p1; addActor: p2.
	self assert: m1 actors size equals: 2.
	self assert: p1 actedInMovies size equals: 1.
	self assert: p1 actedInMovies anyOne equals: m1.
	self assert: p2 actedInMovies anyOne equals: m1.
	
	m2 addActor: p2.
	self assert: p2 actedInMovies size equals: 2.
