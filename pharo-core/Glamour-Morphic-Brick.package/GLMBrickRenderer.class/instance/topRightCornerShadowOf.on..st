private-shadow
topRightCornerShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw top right corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds topRight  - (width@width negated);
		direction: (width@width negated)/1.4;
		radial: true;
	yourself.
	
	aCanvas fillRectangle: (aBrick brickBounds topRight - (width@0) corner: (aBrick brickBounds topRight + (0@width))) fillStyle: gradient.