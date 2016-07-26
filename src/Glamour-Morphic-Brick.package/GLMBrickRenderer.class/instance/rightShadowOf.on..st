private-shadow
rightShadowOf: aBrick on: aCanvas
	|gradient width|
	width := aBrick shadowWidth.
	"draw top shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: aBrick brickBounds rightCenter - (width@0);
		direction: (width@0);
		yourself.
	aCanvas fillRectangle: ((aBrick brickBounds topRight + (width negated@width)) corner: (aBrick brickBounds corner + (width@width negated))) fillStyle: gradient.