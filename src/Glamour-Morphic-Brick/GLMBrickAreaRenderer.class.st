"
I am a special renderer of non-rectangular but polygon bricks
"
Class {
	#name : #GLMBrickAreaRenderer,
	#superclass : #GLMBrickRenderer,
	#category : #Glamour-Morphic-Brick-Renderer
}

{ #category : #geometry }
GLMBrickAreaRenderer >> drawingBounds: aBrick [
	| aRectangle anOffset |
	
	anOffset := aBrick shadowOffset.
	
	aRectangle := aBrick brickBounds expandBy: (aBrick border widthTop + aBrick shadowWidth).
	anOffset isZero
		ifFalse: [ aRectangle := aRectangle quickMerge: (aRectangle translateBy: anOffset) ].
	
	^ aRectangle
]

{ #category : #rendering }
GLMBrickAreaRenderer >> render: aBrick on: aCanvas [
	self renderShadowOf: aBrick on: aCanvas.
	self renderBackground: aBrick on: aCanvas.
	self renderBorder: aBrick on: aCanvas
]

{ #category : #rendering }
GLMBrickAreaRenderer >> renderBackground: aBrick on: aCanvas [

	aCanvas
		drawPolygon: aBrick area vertices
		color: aBrick color
		borderWidth: 0
		borderColor: Color transparent
]

{ #category : #rendering }
GLMBrickAreaRenderer >> renderBorder: aBrick on: aCanvas [

	aCanvas
		drawPolygon: aBrick area vertices
		color: Color transparent
		borderWidth: aBrick border widthTop
		borderColor: aBrick border colorTop
]

{ #category : #'private - shadow' }
GLMBrickAreaRenderer >> renderCornerShadow: aBrick vector: aVector origin: anOrigin on: aCanvas [
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
]

{ #category : #'private - shadow' }
GLMBrickAreaRenderer >> renderShadow: aBrick vector: aVector origin: anOrigin on: aCanvas [
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
]

{ #category : #rendering }
GLMBrickAreaRenderer >> renderShadowOf: aBrick on: aCanvas [
	| anArea aCurrentVector aFirstVector anOrigin |

	"no need to even try drawing if shadow width is zero"
	aBrick shadowWidth = 0
		ifTrue: [ ^ self ].

	anArea := aBrick area.

	aFirstVector := aBrick area vector.
	aCurrentVector := aFirstVector.
	anOrigin := aBrick area origin.

	[ aCurrentVector next ~= aFirstVector ] whileTrue: [ 
		self renderShadow: aBrick vector: aCurrentVector origin: anOrigin on: aCanvas.
		self renderCornerShadow: aBrick vector: aCurrentVector origin: anOrigin on: aCanvas.
		anOrigin := aCurrentVector + anOrigin.
		aCurrentVector := aCurrentVector next ].
	
	self renderShadow: aBrick vector: aCurrentVector origin: anOrigin on: aCanvas.
	self renderCornerShadow: aBrick vector: aCurrentVector origin: anOrigin on: aCanvas.

	aCanvas
		drawPolygon: (anArea vertices collect: [ :each | each + aBrick shadowOffset ])
		color: aBrick shadowColor
		borderWidth: 0
		borderColor: Color transparent
]
