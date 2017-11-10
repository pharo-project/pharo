Extension { #name : #Integer }

{ #category : #'*Graphics-Primitives' }
Integer >> asColorOfDepth: d [
	"Return a color value representing the receiver as color of the given depth"
	^Color colorFromPixelValue: self depth: d
]
