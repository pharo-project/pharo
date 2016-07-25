events
handlesKeyboard: evt from: aTextMorph
	^ (super handlesKeyboard: evt from: aTextMorph)
		or: [selectorChooser notNil and: [selectorChooser handlesKeyboard: evt]]