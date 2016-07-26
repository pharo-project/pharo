tests
testSelect

	| movie result |
	
	movie := SlotExampleMovie named: 'Thelma and Louise'.
	movie
		addActor: (SlotExamplePerson named: 'P1') ;
		addActor: (SlotExamplePerson named: 'P2').

	result := movie actors select: [ :e | e name = 'P1' ].
	self assert: result class equals: Set.
	self assert: result size equals: 1.
	self assert: result anyOne name equals: 'P1'.