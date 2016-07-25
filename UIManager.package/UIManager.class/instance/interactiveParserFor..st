settings
interactiveParserFor: requestor	
	
	requestor ifNil: [^ false ].
	(requestor respondsTo: #interactive)
		ifTrue: [ ^requestor interactive ].
	^true