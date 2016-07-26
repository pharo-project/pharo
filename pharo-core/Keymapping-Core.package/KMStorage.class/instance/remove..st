accessing
remove: aKeymap
	aKeymap hasName
		ifTrue: [ namedRegistry removeKey: aKeymap name ]
		ifFalse: [ anonymousRegistry remove: aKeymap ].