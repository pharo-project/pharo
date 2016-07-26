accessing
add: aKeymap
	aKeymap hasName
		ifTrue: [ namedRegistry at: aKeymap name put: aKeymap ]
		ifFalse: [ anonymousRegistry add: aKeymap ].