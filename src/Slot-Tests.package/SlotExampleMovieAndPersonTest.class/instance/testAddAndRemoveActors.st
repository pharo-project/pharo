tests
testAddAndRemoveActors

	| m1 m2 p1 p2 |
	
	m1 := SlotExampleMovie named: 'M1'.
	m2 := SlotExampleMovie named: 'M2'.
	p1 := SlotExamplePerson named: 'P1'.
	p2 := SlotExamplePerson named: 'P2'.
	
	m1 addActor: p1; addActor: p2.
	m2 addActor: p2.

	m1 removeActor: p2.
	self assert: p2 actedInMovies size equals: 1.
	m2 removeActor: p2.
	self assert: p2 actedInMovies isEmpty.
	
	self should: [ m2 removeActor: p2 ] raise: Error