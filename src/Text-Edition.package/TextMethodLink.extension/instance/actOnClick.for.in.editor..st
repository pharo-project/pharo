*Text-Edition
actOnClick: anEvent for: target in: aParagraph editor: anEditor
	self flag: 'TODO: complete implementation with class'.
	
	browseSenders == anEvent shiftPressed
		ifTrue: [ anEditor implementorsOf: self selector ]
		ifFalse: [ anEditor sendersOf: self selector ].
		
	^ true