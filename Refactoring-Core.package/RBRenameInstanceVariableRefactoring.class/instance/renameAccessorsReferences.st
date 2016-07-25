transforming
renameAccessorsReferences
	| methods senders |
	methods := (class allSelectors select: [ :each | ((class methodFor: each) = nil) not ] thenCollect: [ :each | each ])
		asOrderedCollection.
	senders := (methods
		select: [ :each | (class methodFor: each) source includesSubstring: variableName asString ]
		thenCollect: [ :each | class methodFor: each ]) asOrderedCollection.
	senders
		do: [ :each | each source: (each source copyReplaceAll: variableName asString with: newName asString asTokens: false) ].
	senders
		do: [ :each | 
			(each selector = newName or: [ each selector asString = (newName asString , ':') ])
				ifFalse: [ (model classNamed: class name) compile: each source classified: each protocols ] ]