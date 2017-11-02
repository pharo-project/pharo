Extension { #name : #MultistateButtonMorph }

{ #category : #'*Morphic-Widgets-Windows' }
MultistateButtonMorph >> activate [
	"Make active."

	super activate.
	self active: true
]

{ #category : #'*Morphic-Widgets-Windows' }
MultistateButtonMorph >> passivate [
	"Make passive."

	super passivate.
	self active: false
]
