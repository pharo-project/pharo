accessing
keyText
	"I return the character associated with this shortcut.
	I return an uppercase character if the keymap has shift as a modifier."

	^  self keymap 
		ifNil: [ 
			"Backwards compatibility."
			self defaultKeyText ]
		ifNotNil: [ |currentCharacter|
			"Needed as current menus display 'Shift' only if the charater is in uppercase."
			currentCharacter := self keymap currentCharacter.
			(self keymap printString includesSubstring: 'Shift')
				ifTrue: [  currentCharacter asString asUppercase ]
				ifFalse: [ currentCharacter asString ] ]