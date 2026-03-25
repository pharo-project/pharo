Extension { #name : 'TextLink' }

{ #category : '*Rubric' }
TextLink >> rubMayActOnEvent: anEvent [
	^ anEvent isMouseDown and: [ anEvent metaKeyPressed ]
]
