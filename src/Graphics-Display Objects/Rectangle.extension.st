Extension { #name : 'Rectangle' }

{ #category : '*Graphics-Display Objects' }
Rectangle >> asMargin [

	^ Margin fromRectangle: self
]
