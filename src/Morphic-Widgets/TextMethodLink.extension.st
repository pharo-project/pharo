Extension { #name : 'TextMethodLink' }

{ #category : '*Morphic-Widgets' }
TextMethodLink >> rubActOnClick: anEvent for: target in: aParagraph editor: anEditor [

	self flag: #pharoTodo. "Complete implementation with class"

	self browseSenders == anEvent shiftPressed
		ifTrue: [ anEditor editor implementorsOf: self selector ]
		ifFalse: [ anEditor editor sendersOf: self selector ].

	^ true
]

{ #category : '*Morphic-Widgets' }
TextMethodLink >> rubMayActOnEvent: anEvent [
	^ anEvent isMouseMove or: [ super rubMayActOnEvent: anEvent ]
]
