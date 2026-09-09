Extension { #name : 'Rectangle' }

{ #category : '*Graphics-Display Objects' }
Rectangle >> asMargin [

	^ Margin fromRectangle: self
]

{ #category : '*Graphics-Display Objects' }
Rectangle >> expandBy: delta [
	"Answer a Rectangle that is outset from the receiver by delta. delta is a
	Rectangle, Point, or scalar."

	^ delta asMargin expandRectangle: self
]

{ #category : '*Graphics-Display Objects' }
Rectangle >> extendBy: deltaMargin [
	"Answer a Rectangle with the same origin as the receiver, but whose corner is offset by deltaMargin."

	^ deltaMargin asMargin extendRectangle: self
]

{ #category : '*Graphics-Display Objects' }
Rectangle >> insetBy: delta [
	"Answer a Rectangle that is inset from the receiver by delta. delta is a
	Rectangle, Point, or scalar."

	^ delta asMargin insetRectangle: self
]
