private-shadow
topRightCornerShadowOf: aBrick on: aCanvas
	| gradient width origin offset |
	
	width := aBrick shadowWidth.
	offset := (width @ width) negated / 2.0.
	origin := aBrick brickBounds topRight + offset + aBrick shadowOffset.
	
	"draw top right corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin + (0 @ width);
		direction: (width @ width negated) / 1.4;
		radial: true;
	yourself.
	
	aCanvas fillRectangle: (origin extent: width @ width) fillStyle: gradient.