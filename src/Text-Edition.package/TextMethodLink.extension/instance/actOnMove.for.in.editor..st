*Text-Edition
actOnMove: anEvent for: target in: aParagraph editor: anEditor
	anEvent shiftPressed ifFalse: [ ^ false ].
	anEditor textArea highlightMessageSend ifFalse: [ ^ false ].
	sourceNode ifNil: [ ^ false ].
	anEditor hoverHighlightInterval: self sourceNodeInterval.
	^ true