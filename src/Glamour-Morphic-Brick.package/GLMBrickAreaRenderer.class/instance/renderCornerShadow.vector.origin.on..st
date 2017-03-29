private - shadow
renderCornerShadow: aBrick vector: aVector origin: anOrigin on: aCanvas
	| anInset anOffset aShadowWidth aShadowOrigin aShadowEnd aGradient aRectangle |

	anOffset := aBrick shadowOffset.
	aShadowWidth  := aBrick shadowWidth.

	(aVector rightAngle: aVector next)
		ifTrue: [ 
			aGradient := self shadowGradientReverseFor: aBrick.
			aShadowOrigin := (aVector + anOrigin) + (aVector next * aShadowWidth) + (aVector next left * aShadowWidth).
			aShadowEnd := aVector + anOrigin.
			aRectangle := (aVector similar: aShadowWidth) origin: (aShadowOrigin) breadth: aShadowWidth ]
		ifFalse: [ 
			aGradient := self shadowGradientFor: aBrick.
			aShadowOrigin := aVector + anOrigin.
			aShadowEnd := (aVector + anOrigin) + (aVector left * aShadowWidth) + (aVector * aShadowWidth).
			aRectangle := (aVector left similar: aShadowWidth) origin: (aVector + anOrigin) breadth: aShadowWidth ].

	anInset := (aShadowEnd - aShadowOrigin) sign * aBrick shadowInset.
	
	(aVector rightAngle: aVector next)
		ifTrue: [
			aShadowOrigin := aShadowOrigin - anInset.
			aRectangle := aRectangle translateBy: anInset negated ]
		ifFalse: [
			aShadowOrigin := aShadowOrigin + anInset.
			aRectangle := aRectangle translateBy: anInset ].
	
	aShadowOrigin := aShadowOrigin + anOffset.
	aRectangle := aRectangle translateBy: anOffset.
	
	aGradient
		origin: aShadowOrigin;
		direction: aVector * aShadowWidth;
		radial: true;
		yourself.

	aCanvas fillRectangle: aRectangle fillStyle: aGradient