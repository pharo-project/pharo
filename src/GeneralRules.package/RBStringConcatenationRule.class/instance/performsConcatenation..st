hooks
performsConcatenation: aNode

	aNode isBlock ifFalse: [ ^ false ].
	aNode nodesDo: [ :node |
		(node isMessage and: [ 
		node selector = #, ]) ifTrue: [ 
			^ true ] ].
	
	^ false