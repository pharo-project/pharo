private
modifiesCollection: aCollectionNode inAnyStatement: aStatementCollection 
	aStatementCollection do: [ :statement |
		statement nodesDo: [ :node |
			node = aCollectionNode ifTrue: [ ^ true ] ] ].

	^ false