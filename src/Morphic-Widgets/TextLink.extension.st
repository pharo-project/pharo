Extension { #name : 'TextLink' }

{ #category : '*Morphic-Widgets' }
TextLink >> rubMayActOnEvent: anEvent [
	^ anEvent isMouseDown and: [ anEvent metaKeyPressed ]
]
