"
A GLMBrickRenderer is most simple separate renderer of Bricks. It support only borders

"
Class {
	#name : #GLMBrickRenderer,
	#superclass : #Object,
	#category : #'Glamour-Morphic-Brick-Renderer'
}

{ #category : #'private-shadow' }
GLMBrickRenderer >> bottomLeftCornerShadowOf: aBrick on: aCanvas [
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
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> bottomRightCornerShadowOf: aBrick on: aCanvas [
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
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> bottomShadowOf: aBrick on: aCanvas [
	| gradient width origin offset |
	
	width := aBrick shadowWidth.
	offset := (width @ width negated) / 2.0.
	origin := aBrick brickBounds bottomLeft + offset + aBrick shadowOffset.

	"draw bottom shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin - offset - (0 @ (width / 2.0));
		direction: 0 @ width;
		yourself.

	aCanvas fillRectangle: (origin extent: (aBrick width - width) @ width) fillStyle: gradient
]

{ #category : #geometry }
GLMBrickRenderer >> drawingBounds: aBrick [
	| aRectangle anOffset |

	aRectangle := aBrick brickBounds.
	anOffset := aBrick shadowOffset.

	^ aBrick shadowWidth isZero
		ifTrue: [ aRectangle ]
		ifFalse: [ aRectangle quickMerge: ((aRectangle expandBy: aBrick shadowWidth) translateBy: anOffset) ]
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> leftShadowOf: aBrick on: aCanvas [
	| gradient width origin offset |
	
	width := aBrick shadowWidth.
	offset := (width negated @ width) / 2.0.
	origin := aBrick brickBounds topLeft + offset + aBrick shadowOffset.

	"draw left shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin - offset + (width / 2.0 @ 0);
		direction: (width @ 0) negated;
		yourself.

	aCanvas fillRectangle: (origin extent: width @ (aBrick height - width)) fillStyle: gradient.
]

{ #category : #rendering }
GLMBrickRenderer >> render: aBrick on: aCanvas [
	
	self
		renderShadowOf: aBrick on: aCanvas;
		renderBackgroundColorOf: aBrick on: aCanvas;
		renderBorderOf: aBrick on: aCanvas
]

{ #category : #'rendering athens' }
GLMBrickRenderer >> render: aBrick onAthensCanvas: aCanvas [
	
	self render: aBrick on: (AthensCanvasWrapper on: aCanvas)
]

{ #category : #rendering }
GLMBrickRenderer >> renderBackgroundColorOf: aBrick on: aCanvas [
	| rectangle |

	rectangle := aBrick brickBounds expandBy: aBrick shadowWidth negated.
	self renderBackgroundColorOf: aBrick on: aCanvas in: rectangle
]

{ #category : #private }
GLMBrickRenderer >> renderBackgroundColorOf: aBrick on: aCanvas in: aRectangle [

	aCanvas fillRectangle: aRectangle color: aBrick color.
	(aBrick fillStyle = Color transparent) ifFalse: [ 
		aCanvas fillRectangle: aRectangle fillStyle: aBrick fillStyle borderStyle: aBrick borderStyle ]
]

{ #category : #rendering }
GLMBrickRenderer >> renderBorderOf: aBrick on: aCanvas [
	| markColor markWidth |
	aBrick border renderBorderOf: aBrick on: aCanvas.
	
	markColor := aBrick valueOfProperty: #brickMarkBorderColor ifAbsent: nil.
	markWidth := aBrick valueOfProperty: #brickMarkBorderWidth ifAbsent: nil.
	
	markColor isNotNil & markWidth isNotNil ifTrue: [
		aCanvas frameRectangle: aBrick brickBounds width: markWidth color: markColor ]
]

{ #category : #rendering }
GLMBrickRenderer >> renderShadowOf: aBrick on: aCanvas [

	"no need to even try drawing if shadow width is zero"
	aBrick shadowWidth = 0
		ifTrue: [ ^ self ].

	self
		topLeftCornerShadowOf: aBrick on: aCanvas;
		topRightCornerShadowOf: aBrick on: aCanvas;
		bottomRightCornerShadowOf: aBrick on: aCanvas;
		bottomLeftCornerShadowOf: aBrick on: aCanvas;
		leftShadowOf: aBrick on: aCanvas;
		topShadowOf: aBrick on: aCanvas;
		rightShadowOf: aBrick on: aCanvas;
		bottomShadowOf: aBrick on: aCanvas.
		
	aCanvas
		fillRectangle: ((aBrick brickBounds insetBy: aBrick shadowWidth / 2.0) translateBy: aBrick shadowOffset)
		color: aBrick shadowColor
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> rightShadowOf: aBrick on: aCanvas [
	| gradient width origin |

	width := aBrick shadowWidth.
	origin := aBrick brickBounds topRight + ((width negated @ width) / 2.0) + aBrick shadowOffset.

	"draw right shadow"
	gradient := (self shadowGradientFor: aBrick)
		origin: origin;
		direction: (width@0);
		yourself.

	aCanvas fillRectangle: (origin extent: width @ (aBrick height - width)) fillStyle: gradient
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> shadowGradientFor: aBrick [

	^ GradientFillStyle ramp: { 0.0 -> (aBrick shadowColor) . 1.0 -> (aBrick shadowColor alpha: 0)}
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> shadowGradientReverseFor: aBrick [

	^ GradientFillStyle ramp: { 0.0 -> (aBrick shadowColor alpha: 0). 1.0 -> (aBrick shadowColor) }
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> topLeftCornerShadowOf: aBrick on: aCanvas [
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
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> topRightCornerShadowOf: aBrick on: aCanvas [
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
]

{ #category : #'private-shadow' }
GLMBrickRenderer >> topShadowOf: aBrick on: aCanvas [
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
]
