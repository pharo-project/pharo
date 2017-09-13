hooks
sendsAtWith: aVariableNode inAnyOf: aStatementCollection

	aStatementCollection do: [ :statement |
		statement nodesDo: [ :node |
			(node isMessage and: [ 
			node selector = #at: and: [ 
			node arguments first = aVariableNode ] ])
				ifTrue: [ ^ true ] ] ].
		
	^ false