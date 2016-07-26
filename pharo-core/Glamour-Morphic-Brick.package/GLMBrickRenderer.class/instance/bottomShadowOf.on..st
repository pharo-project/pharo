private-shadow
bottomShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw bottom shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds bottomCenter - (0@width);
		direction: (0@width);
		yourself.
	aCanvas fillRectangle: ((aBrick brickBounds bottomLeft + (width@width negated)) corner: (aBrick brickBounds bottomLeft - (width @width) + (aBrick brickBounds width @ width))) fillStyle: gradient.