rendering
renderShadowOf: aBrick on: aCanvas
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