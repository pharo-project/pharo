Extension { #name : 'SVColorSelectorMorph' }

{ #category : '*Morphic-Widgets' }
SVColorSelectorMorph >> borderWidth: anInteger [
	"Update the gradients after setting."

	super borderWidth: anInteger.
	self updateGradients
]
