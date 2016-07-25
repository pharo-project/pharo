events
keystroke: aKeyboardEvent from: aTextMorph
	| asciiValue |
	selectorChooser 
		ifNotNil: [(selectorChooser handlesKeyStroke: aKeyboardEvent) 
			ifTrue: [selectorChooser keyStroke: aKeyboardEvent.
				^ true]].
	asciiValue := aKeyboardEvent keyValue.
	asciiValue = Character cr asciiValue
		ifTrue: [self closeSelectorChooser.
			^ true].
	aKeyboardEvent controlKeyPressed
		ifTrue: [self closeSelectorChooser.
			^ true].
	"aKeyboardEvent commandKeyPressed
		ifTrue: [self closeSelectorChooser.
			^ true]."
	self updateSelectorChooserFrom: aTextMorph.
	^ false