Extension { #name : #TextLink }

{ #category : #'*Rubric-Editing-Core' }
TextLink >> rubMayActOnEvent: anEvent [
	^ anEvent isMouseDown and: [ anEvent commandKeyPressed ]
]
