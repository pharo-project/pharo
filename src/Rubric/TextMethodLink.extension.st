Extension { #name : #TextMethodLink }

{ #category : #'*Rubric-Editing-Core' }
TextMethodLink >> rubActOnClick: anEvent for: target in: aParagraph editor: anEditor [
	self flag: #pharoTodo. "Complete implementation with class".
	
	browseSenders == anEvent shiftPressed
		ifTrue: [ anEditor editor implementorsOf: self selector ]
		ifFalse: [ anEditor editor sendersOf: self selector ].
		
	^ true
]

{ #category : #'*Rubric-Editing-Core' }
TextMethodLink >> rubMayActOnEvent: anEvent [
	^ anEvent isMouseMove or: [ anEvent isMouseDown and: [ anEvent commandKeyPressed ] ]
]
