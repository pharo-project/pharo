*Text-Edition
actOnClick: anEvent for: target in: aParagraph editor: anEditor
	anEvent shiftPressed
		ifFalse: [ anEditor browseClassFrom: self className ]
		ifTrue: [ anEditor referencesTo: self className ].
	^ true