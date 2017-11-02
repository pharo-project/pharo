Extension { #name : #FillStyle }

{ #category : #'*Balloon' }
FillStyle >> fillRectangle: aRectangle on: aCanvas [
	"Fill the given rectangle on the given canvas with the receiver."
	
	aCanvas fillRectangle: aRectangle basicFillStyle: self
]
