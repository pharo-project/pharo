private-shadow
bottomRightCornerShadowOf: aBrick on: aCanvas
	| gradient width origin offset |

	width := aBrick shadowWidth.
	offset := (width @ width) negated / 2.0.
	origin := aBrick brickBounds bottomRight + offset + aBrick shadowOffset.

	"draw top right corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin;
		direction: (width @ width) / 1.4;
		radial: true;
	yourself.

	aCanvas fillRectangle: (origin extent: width @ width) fillStyle: gradient.