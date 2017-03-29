private-shadow
leftShadowOf: aBrick on: aCanvas
	| gradient width origin offset |
	
	width := aBrick shadowWidth.
	offset := (width negated @ width) / 2.0.
	origin := aBrick brickBounds topLeft + offset + aBrick shadowOffset.

	"draw left shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin - offset + (width / 2.0 @ 0);
		direction: (width @ 0) negated;
		yourself.

	aCanvas fillRectangle: (origin extent: width @ (aBrick height - width)) fillStyle: gradient.