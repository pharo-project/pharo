Extension { #name : #TextLink }

{ #category : #'*Text-Edition' }
TextLink >> mayActOnEvent: anEvent [
	^ anEvent isMouseUp and: [ anEvent commandKeyPressed ]
]
