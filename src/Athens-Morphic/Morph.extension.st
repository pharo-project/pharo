Extension { #name : #Morph }

{ #category : #'*Athens-Morphic' }
Morph >> athensSurface [
	^ owner ifNotNil: [ owner athensSurface ]
]

{ #category : #'*Athens-Morphic' }
Morph >> drawClippedOnAthensCanvas: anAthensCanvas during: aBlock [

	^ self clipSubmorphs
		ifTrue: [
			 anAthensCanvas
				clipBy: (self clippingBounds intersect: anAthensCanvas clipRect ifNone:[^ self])
				during: aBlock ]
		ifFalse: [ aBlock value ]
]

{ #category : #'*Athens-Morphic' }
Morph >> drawKeyboardFocusOnAthensCanvas: anAthensCanvas [
	"Draw the keyboard focus indication."

	self focusIndicatorMorph
		drawOnAthensCanvas: anAthensCanvas
]

{ #category : #'*Athens-Morphic' }
Morph >> drawOnAthensCanvas: anAthensCanvas [
	| border |
	border := self borderStyle.
	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas drawShape: self bounds.

	"Fill the given rectangle."
	anAthensCanvas drawShape: (self bounds insetBy: border width).

	(anAthensCanvas setStrokePaint: border).
	anAthensCanvas drawShape: (self bounds insetBy: (border width /2 asFloat)).


	"aBorderStyle frameRectangle: aRectangle on: self "
]

{ #category : #'*Athens-Morphic' }
Morph >> drawOnCanvasWrapperFor: anAthensCanvas [
	self drawOn: anAthensCanvas asCanvasWrapper
]

{ #category : #'*Athens-Morphic' }
Morph >> drawSubmorphsOnAthensCanvas: anAthensCanvas [
	self
		drawClippedOnAthensCanvas: anAthensCanvas
		during: [ submorphs reverseDo: [ :m | anAthensCanvas fullDrawMorph: m ] ]
]

{ #category : #'*Athens-Morphic' }
Morph >> fullDrawOnAthensCanvas: anAthensCanvas [
	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(anAthensCanvas isVisible: self fullBounds) ifFalse: [^self].
	(self hasProperty: #errorOnDraw) ifTrue: [^self "drawErrorOn: aCanvas" ].

	(anAthensCanvas isVisible: self bounds) ifTrue: [ anAthensCanvas draw: self ].

	submorphs isEmpty ifTrue: [^self].

	self drawSubmorphsOnAthensCanvas: anAthensCanvas
]

{ #category : #'*Athens-Morphic' }
Morph >> testAthensRender [

| surf |
	surf := AthensCairoSurface extent: Display extent.

	surf drawDuring: [:can |
"		can pathTransform scaleBy: 0.5; rotateByDegrees: 30."
		self fullDrawOnAthensCanvas: can ].

	Display getCanvas drawImage: 	surf asForm  at: 0@0
]
