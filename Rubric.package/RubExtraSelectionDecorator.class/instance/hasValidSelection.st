testing
hasValidSelection

	| paragraph |
	
	paragraph := self paragraph ifNil: [ ^false ].
	
	^paragraph selectionStart notNil and: [ 
		paragraph selectionStop notNil and: [ 
			paragraph selectionStart ~= paragraph selectionStop 
		] 
	]