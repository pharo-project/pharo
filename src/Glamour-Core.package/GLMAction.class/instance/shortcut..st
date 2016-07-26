accessing
shortcut: aCharacter 
	"Shortcut character that this action should be triggered with when the focus rests on the corresponding presentation. 
	Modifiers are platform specific an can not be defined." 
	 
	aCharacter ifNil: [ 
		keymap := nil.
		^ self ].
	keymap := 
		(aCharacter isUppercase 
			ifTrue: [ aCharacter meta shift ]
			ifFalse: [ aCharacter meta ])