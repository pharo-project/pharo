private-shadow
bottomLeftCornerShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw top left corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds bottomLeft + (width@width negated);
		direction: (width negated@width)/1.4;
		radial: true;
		yourself.
	aCanvas fillRectangle: (aBrick brickBounds bottomLeft - (0@width) corner: (aBrick brickBounds bottomLeft + (width@0))) fillStyle: gradient.