private-shadow
bottomRightCornerShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw top right corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds corner  - (width@width);
		direction: (width@width)/1.4;
		radial: true;
	yourself.
	
	aCanvas fillRectangle: (aBrick brickBounds corner  - (width@width) corner: (aBrick brickBounds corner)) fillStyle: gradient.