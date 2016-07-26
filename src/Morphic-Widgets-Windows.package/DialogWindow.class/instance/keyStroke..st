event handling
keyStroke: evt
	"Check for return and escape keys."

	super keyStroke: evt.
	(self defaultButton notNil and: [evt keyCharacter = Character cr]) ifTrue: [self returnPressed. ^true].
	evt keyCharacter = Character escape ifTrue: [self escapePressed. ^true].
	^false