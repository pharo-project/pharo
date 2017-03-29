private-shadow
bottomShadowOf: aBrick on: aCanvas
	| gradient width origin offset |
	
	width := aBrick shadowWidth.
	offset := (width @ width negated) / 2.0.
	origin := aBrick brickBounds bottomLeft + offset + aBrick shadowOffset.

	"draw bottom shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin - offset - (0 @ (width / 2.0));
		direction: 0 @ width;
		yourself.

	aCanvas fillRectangle: (origin extent: (aBrick width - width) @ width) fillStyle: gradient