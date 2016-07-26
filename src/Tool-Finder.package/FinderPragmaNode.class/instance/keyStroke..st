event handling
keyStroke: anEvent
	(anEvent controlKeyPressed or: [ anEvent commandKeyPressed ]) ifFalse: [ ^ false ].
	anEvent keyCharacter == $h ifTrue: [ ^ self browseHierarchy ].
	anEvent keyCharacter == $N ifTrue: [ ^ self browseReferences ].
	^ super keyStroke: anEvent.