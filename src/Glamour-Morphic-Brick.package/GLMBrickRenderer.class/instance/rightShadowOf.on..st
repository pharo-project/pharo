private-shadow
rightShadowOf: aBrick on: aCanvas
	| gradient width origin |

	width := aBrick shadowWidth.
	origin := aBrick brickBounds topRight + ((width negated @ width) / 2.0) + aBrick shadowOffset.

	"draw right shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin;
		direction: (width@0);
		yourself.

	aCanvas fillRectangle: (origin extent: width @ (aBrick height - width)) fillStyle: gradient