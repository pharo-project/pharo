private - shadow
renderShadow: aBrick vector: aVector origin: anOrigin on: aCanvas
	| anInset anOffset aShadowWidth aShadowOrigin aShadowVector aShadowLength aGradient aRectangle |

	anInset := aBrick shadowInset.
	anOffset := aBrick shadowOffset.

	aShadowWidth  := aBrick shadowWidth.
	aShadowVector := aVector left length: aShadowWidth.
	
	aShadowLength := aVector length + (anInset * 2.0).

	((aVector rightAngle: aVector next) or: [ aVector previous rightAngle: aVector ])
		ifTrue: [ aShadowLength := aShadowLength - aShadowWidth + (anInset negated * 2 ) ].

	aShadowOrigin := anOrigin + anOffset + (aShadowVector * anInset) - (aVector * anInset).
	(aVector previous rightAngle: aVector)
		ifTrue: [ aShadowOrigin := aShadowOrigin + (aVector * aShadowWidth) + (aVector * anInset * 2) ].

	aGradient := (self shadowGradientFor: aBrick)
		origin: aShadowOrigin;
		direction: aShadowVector * aShadowWidth;
		yourself.

	aRectangle := aShadowVector origin: aShadowOrigin breadth: aShadowLength.

	aCanvas fillRectangle: aRectangle fillStyle: aGradient.
	"aCanvas frameRectangle: aRectangle color: Color red"