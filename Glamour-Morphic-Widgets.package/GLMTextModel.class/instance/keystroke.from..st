callbacks
keystroke: anEvent from: aMorph
	anEvent anyModifierKeyPressed ifTrue: [ ^ super keystroke: anEvent from: aMorph ].
	self glamourPresentation text: self text.
	^ false