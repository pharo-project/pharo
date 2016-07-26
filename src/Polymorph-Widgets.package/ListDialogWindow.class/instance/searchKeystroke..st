events
searchKeystroke: event
	
	searchMorph content ifNil: [ ^ false ].
	
	(event keyCharacter = Character arrowDown )
		ifTrue: [ ^ self searchKeystrokeDown ].
		
	(event keyCharacter = Character arrowUp)
		ifTrue: [ ^ self searchKeystrokeUp ].
	