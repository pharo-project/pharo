tests
testSuchThat
	| q |
	q := self newQueue.
	
	#( 1 2 3 4 5 6 7 8 9 10 ) do: [:each |
		q nextPut: each ].

	self assert: (q nextOrNilSuchThat: [:e | e = 100 ]) isNil.	

	self assert: (q nextOrNilSuchThat: [:e | e = 5 ]) = 5.	

	9 timesRepeat: [ 
		self assert: (q nextOrNil notNil) ].
	
	self assert: q nextOrNil isNil