*Rubric-Editing-Core
rubActOnClick: anEvent for: target in: aParagraph editor: anEditor
	anEvent shiftPressed
		ifFalse: [ anEditor editor browseClassFrom: self className ]
		ifTrue: [ anEditor editor referencesTo: self className ].
	^ true