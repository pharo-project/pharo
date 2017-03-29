private-shadow
bottomLeftCornerShadowOf: aBrick on: aCanvas
	| gradient width origin offset |

	width := aBrick shadowWidth.
	offset := (width @ width) negated / 2.0.
	origin := aBrick brickBounds bottomLeft + offset + aBrick shadowOffset.

	"draw top left corner"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin + (width @ 0);
		direction: (width negated @ width) / 1.4;
		radial: true;
		yourself.

	aCanvas fillRectangle: (origin extent: width @ width) fillStyle: gradient