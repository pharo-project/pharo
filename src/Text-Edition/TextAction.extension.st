Extension { #name : 'TextAction' }

{ #category : '*Text-Edition' }
TextAction >> actOnMove: anEvent for: target in: aParagraph editor: anEditor [
	"override in subclasses"
	^ false
]
