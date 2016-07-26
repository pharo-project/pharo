transforming
formNameFor: aSelector
	| method keywords arguments |
	method := class methodFor: aSelector.
	keywords := method parseTree keywords asOrderedCollection ifNil: [ '' ].
	arguments := method parseTree arguments ifNil: [ '' ].

	(keywords size >= 1 and: [ arguments size >= 1 ]) 
		ifTrue: [ ^ String streamContents: [ :s| 
			keywords with: arguments do: [ :keyword :arg | s nextPutAll: keyword, ' ', arg name, ' ' ] ] ]
		ifFalse: [ ^ aSelector asString ].
	
