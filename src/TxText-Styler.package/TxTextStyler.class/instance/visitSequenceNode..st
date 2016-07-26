visiting
visitSequenceNode: node
	
	node temporaries do: [ :temporary|
		self 
			addStyle: #patternTempVar
			forNode: temporary ].
	
	node statements do: [:each | self visitNode: each] 
