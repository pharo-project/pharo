basic-completion
selectorChooserKeystroke: aKeyboardEvent
	| asciiValue |
	selectorChooser 
		ifNotNil: [(selectorChooser handlesKeyStroke: aKeyboardEvent) 
			ifTrue: [selectorChooser keyStroke: aKeyboardEvent.
				^ true]].
	asciiValue := aKeyboardEvent keyValue.
	asciiValue = Character cr asciiValue
		ifTrue: [self selectorChooserClose.
			^ true].
	aKeyboardEvent controlKeyPressed
		ifTrue: [self selectorChooserClose.
			^ true].
	"aKeyboardEvent commandKeyPressed
		ifTrue: [self closeSelectorChooser.
			^ true]."
	self selectorChooserUpdate.
	^ false