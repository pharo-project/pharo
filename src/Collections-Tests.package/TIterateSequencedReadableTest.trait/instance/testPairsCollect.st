tests - iterate on sequenced reable collections
testPairsCollect
	
	| index result |
	index:=0.
	
	result:=self nonEmptyMoreThan1Element  pairsCollect: 
		[:each1 :each2 | 
		self assert: ( self nonEmptyMoreThan1Element indexOf: each2 ) = (index := index + 2).
		(self nonEmptyMoreThan1Element indexOf: each2) = ((self nonEmptyMoreThan1Element indexOf: each1) + 1).
		].
	
	result do: 
		[:each | self assert: each = true].
	
