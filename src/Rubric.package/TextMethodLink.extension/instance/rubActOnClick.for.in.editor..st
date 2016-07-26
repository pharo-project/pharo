*Rubric-Editing-Core
rubActOnClick: anEvent for: target in: aParagraph editor: anEditor
	self flag: 'TODO: complete implementation with class'.
	
	browseSenders == anEvent shiftPressed
		ifTrue: [ anEditor editor implementorsOf: self selector ]
		ifFalse: [ anEditor editor sendersOf: self selector ].
		
	^ true