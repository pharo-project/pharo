private-temporaries
convertBlocksToVariables: anOrderedCollection 
	| result blockStack |
	blockStack := anOrderedCollection.
	result := OrderedCollection new.
	blockStack do: [ :each | result addAll: each ].
	^ result