private-shadow
topLeftCornerShadowOf: aBrick on: aCanvas
	| gradient width origin offset extent |
	
	width := aBrick shadowWidth.
	extent := width @ width.
	offset := extent negated / 2.0.
	origin := aBrick brickBounds origin + offset + aBrick shadowOffset.
	
	"draw top left corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin + (offset * 2 negated);
		direction: extent / 1.4 negated;
		radial: true;
		yourself.
	aCanvas fillRectangle: (origin extent: extent) fillStyle: gradient.