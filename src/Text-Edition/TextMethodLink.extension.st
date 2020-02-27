Extension { #name : #TextMethodLink }

{ #category : #'*Text-Edition' }
TextMethodLink >> actOnClick: anEvent for: target in: aParagraph editor: anEditor [
	self flag: #pharoTodo. "Complete implementation with class".
	
	browseSenders == anEvent shiftPressed
		ifTrue: [ anEditor implementorsOf: self selector ]
		ifFalse: [ anEditor sendersOf: self selector ].
		
	^ true
]

{ #category : #'*Text-Edition' }
TextMethodLink >> actOnMove: anEvent for: target in: aParagraph editor: anEditor [
	anEvent shiftPressed ifFalse: [ ^ false ].
	anEditor textArea highlightMessageSend ifFalse: [ ^ false ].
	sourceNode ifNil: [ ^ false ].
	anEditor hoverHighlightInterval: self sourceNodeInterval.
	^ true
]
