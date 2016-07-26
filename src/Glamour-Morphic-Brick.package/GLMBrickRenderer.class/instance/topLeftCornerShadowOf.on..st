private-shadow
topLeftCornerShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw top left corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds origin + (width@width);
		direction: (width@width)/1.4 negated;
		radial: true;
		yourself.
	aCanvas fillRectangle: (aBrick brickBounds origin corner: (aBrick brickBounds origin + (width@width))) fillStyle: gradient.