event handling
keystrokeFromTextMorph: anEvent
	anEvent keyCharacter = Character cr
		ifTrue: [self closeChooser].
	^ (super keystrokeFromTextMorph: anEvent)
		or: [entryCompletion notNil and: [entryCompletion keystroke: anEvent from: self]]
			
	