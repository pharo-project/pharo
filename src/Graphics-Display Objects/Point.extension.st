Extension { #name : 'Point' }

{ #category : '*Graphics-Display Objects' }
Point >> asMargin [
	"Return a margin instance based on the receiver. aMargin is an object representing either 1, 2 or 4 numbers. It represents the space between a rectangular area and this rectangular area augmented by the margin"

	^ Margin fromPoint: self
]
