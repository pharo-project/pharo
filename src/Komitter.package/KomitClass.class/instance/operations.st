protocol
operations
	
	^ (definition
		ifNil: [ operations ]
		ifNotNil: [ {definition}, operations asOrderedCollection ]) asOrderedCollection