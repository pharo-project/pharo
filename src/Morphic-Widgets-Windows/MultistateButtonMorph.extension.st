Extension { #name : 'MultistateButtonMorph' }

{ #category : '*Morphic-Widgets-Windows' }
MultistateButtonMorph >> activate [
	"Make active."

	super activate.
	self active: true
]
