events handling
keyStroke: anEvent
	(anEvent controlKeyPressed or: [ anEvent commandKeyPressed ]) ifFalse: [ ^ false ].
	anEvent keyCharacter == $b ifTrue: [ ^ self browse ].
	anEvent keyCharacter == $i ifTrue: [ ^ self inspectItem ].