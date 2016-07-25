events handling
keyStroke: anEvent
	(anEvent controlKeyPressed or: [ anEvent commandKeyPressed ]) ifFalse: [ ^ false ].
	anEvent keyCharacter == $n ifTrue: [ ^ self browseSenders ].
	anEvent keyCharacter == $m ifTrue: [ ^ self browseImplementors ].
	^ super keyStroke: anEvent.