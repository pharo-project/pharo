private-shadow
leftShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw left shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds leftCenter + (width@0);
		direction: (width@0)negated;
		yourself.
	aCanvas fillRectangle: ((aBrick brickBounds origin + (0@width)) corner: (aBrick brickBounds origin + (width@width negated) + (0@ aBrick brickBounds height))) fillStyle: gradient.