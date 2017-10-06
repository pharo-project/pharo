operating
fillRectangle: aRectangle on: aCanvas
	"Fill the given rectangle on the given canvas with the receiver."
	
	|o c|
	o := self origin ifNil: [aRectangle origin] ifNotNil: [self origin].
	c := self extent ifNil: [aRectangle corner] ifNotNil: [o + self extent].
	aCanvas fillRectangle: (o corner: c) basicFillStyle: self