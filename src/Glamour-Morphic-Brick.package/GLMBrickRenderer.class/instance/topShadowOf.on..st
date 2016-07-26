private-shadow
topShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw top shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds topCenter + (0@width);
		direction: (0@width)negated;
		yourself.
		
	aCanvas fillRectangle: ((aBrick brickBounds origin + (width@0)) corner: (aBrick brickBounds origin - (width@0) + (aBrick brickBounds width @ width))) fillStyle: gradient.