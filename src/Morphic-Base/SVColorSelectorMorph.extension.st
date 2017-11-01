Extension { #name : #SVColorSelectorMorph }

{ #category : #'*Morphic-Base' }
SVColorSelectorMorph >> borderWidth: anInteger [
	"Update the gradients after setting."
	
	super borderWidth: anInteger.
	self updateGradients
]
