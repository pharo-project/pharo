*Morphic-Base
dispatchKeystroke: anEvent
	"A keystroke was hit when no keyboard focus was set, so it is sent here to the world instead."

	|  aChar ascii |
	aChar := anEvent keyCharacter.
	(ascii := aChar asciiValue) = 27 ifTrue: "escape key"
		[^ self invokeWorldMenuFromEscapeKey].
	
	(self navigationKey: anEvent) ifTrue: [^self].

	(anEvent commandKeyPressed and: [Editor cmdKeysInText])
		ifTrue: [^ self dispatchCommandKeyInWorld: aChar event: anEvent].

	"It was unhandled. Remember the keystroke."
	self lastKeystroke: anEvent keyString.
	self triggerEvent: #keyStroke.
	super dispatchKeystroke: anEvent