private-shadow
topShadowOf: aBrick on: aCanvas
	| gradient width origin offset |

	width := aBrick shadowWidth.
	offset := (width @ width negated) / 2.0.
	origin := aBrick brickBounds topLeft + offset + aBrick shadowOffset.

	"draw top shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin - offset + (0 @ (width / 2.0));
		direction: (0 @ width) negated;
		yourself.
		
	aCanvas fillRectangle: (origin extent: (aBrick width - width) @ width) fillStyle: gradient.