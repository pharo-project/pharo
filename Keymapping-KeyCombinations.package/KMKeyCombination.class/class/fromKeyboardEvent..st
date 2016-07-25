instance creation
fromKeyboardEvent: evt
	| modifier control command shift alt |
	control := evt controlKeyPressed.
	command := evt commandKeyPressed.
	shift := evt shiftPressed.
	alt := evt altKeyPressed.
	(shift | command | control | alt)
		ifFalse: [^ KMSingleKeyCombination from: evt modifiedCharacter ].
	modifier := KMNoShortcut new.
	control
		ifTrue: [ modifier := modifier + KMModifier ctrl ].
	command
		ifTrue: [ modifier := modifier + KMModifier command ].
	shift
		ifTrue: [ modifier := modifier + KMModifier shift ].
	alt
		ifTrue: [ modifier := modifier + KMModifier alt ].
	^ modifier + evt modifiedCharacter