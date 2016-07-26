visiting rb nodes
visitSequenceNode: aSequenceNode 
	aSequenceNode temporaries do: [ :temporary|
		self 
			addStyle: #patternTempVar
			forNode: temporary ].
	
	aSequenceNode statements do: [:each | self visitNode: each]