"
A balloon with text used for the display of explanatory information.

Balloon help is integrated into Morphic as follows:
If a Morph has the property #balloonText, then it will respond to #showBalloon by adding a text balloon to the world, and to #deleteBalloon by removing the balloon.

Moreover, if mouseOverEnabled is true (see class msg), then the Hand will arrange to cause display of the balloon after the mouse has lingered over the morph for a while, and removal of the balloon when the mouse leaves the bounds of that morph.  In any case, the Hand will attempt to remove any such balloons before handling mouseDown events, or displaying other balloons.

Balloons should not be duplicated with veryDeepCopy unless their target is also duplicated at the same time.
"
Class {
	#name : #BalloonMorph,
	#superclass : #PolygonMorph,
	#instVars : [
		'target',
		'offsetFromTarget',
		'balloonOwner',
		'balloonColor'
	],
	#classVars : [
		'BalloonColor',
		'BalloonFont'
	],
	#category : #'Morphic-Base-Widgets'
}

{ #category : #utilities }
BalloonMorph class >> balloonColor [
	^ BalloonColor ifNil: [BalloonColor := self defaultBalloonColor]
]

{ #category : #utilities }
BalloonMorph class >> balloonFont [
	^ StandardFonts balloonFont
]

{ #category : #utilities }
BalloonMorph class >> balloonFont: aFont [
	StandardFonts balloonFont: aFont
]

{ #category : #private }
BalloonMorph class >> defaultBalloonColor [

	^ (Color fromArray: #(0.85 0.9 1.0 )) twiceLighter alpha: 0.95
]

{ #category : #private }
BalloonMorph class >> getBestLocation: vertices for: morph corner: cornerName [
	| rect maxArea verts rectCorner morphPoint mbc a mp dir bestVerts result usableArea |
	"Choose rect independantly of vertice order or size. Would be nice it this took into account curveBounds but it does not." 
	rect := Rectangle encompassing: vertices.  
	maxArea := -1.
	verts := vertices.
	usableArea := (morph world ifNil: [self currentWorld]) viewBox.
	1 to: 4 do: [:i |
		dir := #(vertical horizontal) atWrap: i.
		verts := verts collect: [:p | p flipBy: dir centerAt: rect center].
		rectCorner := #(bottomLeft bottomRight topRight topLeft) at: i.
		morphPoint := #(topCenter topCenter bottomCenter bottomCenter) at: i.
		a := ((rect
			align: (rect perform: rectCorner)
			with: (mbc := morph boundsForBalloon perform: morphPoint))
				intersect: usableArea) area.
		(a > maxArea or: [a = rect area and: [rectCorner = cornerName]]) ifTrue:
			[maxArea := a.
			bestVerts := verts.
			mp := mbc]].
	result := bestVerts collect: [:p | p + (mp - bestVerts first)] "Inlined align:with:".
	^ result
]

{ #category : #private }
BalloonMorph class >> getTextMorph: aStringOrMorph for: balloonOwner [
	"Construct text morph."
	| m text |
	aStringOrMorph isMorph
		ifTrue: [m := aStringOrMorph]
		ifFalse: [balloonOwner balloonFont
				ifNil: [text := aStringOrMorph]
				ifNotNil: [text := Text
								string: aStringOrMorph
								attribute: (TextFontReference toFont: balloonOwner balloonFont)].
			m := (TextMorph new contents: text) centered; color: UITheme current balloonTextColor].
	m setToAdhereToEdge: #adjustedCenter.
	^ m
]

{ #category : #private }
BalloonMorph class >> getVertices: bounds [
	"Construct vertices for a balloon up and to left of anchor"

	| corners |
	corners := bounds corners atAll: #(1 4 3 2).
	^ (Array
		with: corners first + (0 - bounds width // 2 @ 0)
		with: corners first + (0 - bounds width // 4 @ (bounds height // 2))) , corners
]

{ #category : #'instance creation' }
BalloonMorph class >> initialize [ 
	"BalloonMorph initialize"
	self setBalloonColorTo: BalloonMorph defaultBalloonColor.
	self balloonFont: StandardFonts defaultFont
]

{ #category : #utilities }
BalloonMorph class >> setBalloonColorTo: aColor [ 
	aColor ifNotNil: [BalloonColor := aColor]
]

{ #category : #'instance creation' }
BalloonMorph class >> string: str for: morph [
	^ self string: str for: morph corner: #bottomLeft
]

{ #category : #'instance creation' }
BalloonMorph class >> string: str for: morph corner: cornerName [ 
	"Make up and return a balloon for morph. Find the quadrant that 
	clips the text the least, using cornerName as a tie-breaker. tk 9/12/97"
	| tm vertices |
	tm := self getTextMorph: str for: morph.
	tm composeToBounds. 
	vertices := self getVertices: tm bounds.
	vertices := self
				getBestLocation: vertices
				for: morph
				corner: cornerName.
	^ self new color: self balloonColor;
		 setVertices: vertices;
		 addMorph: tm;
		 setTarget: morph
]

{ #category : #menus }
BalloonMorph >> adjustedCenter [
	"Return the center of the original textMorph box within the balloon."

	^ (self vertices last: 4) average rounded  
]

{ #category : #accessing }
BalloonMorph >> balloonColor [

	^ balloonColor
]

{ #category : #accessing }
BalloonMorph >> balloonColor: aColor [

	balloonColor := aColor.
	self color: aColor
]

{ #category : #accessing }
BalloonMorph >> balloonOwner [
	^balloonOwner
]

{ #category : #initialization }
BalloonMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ self defaultColor muchDarker"Color black"
]

{ #category : #initialization }
BalloonMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 1
]

{ #category : #initialization }
BalloonMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ self balloonColor
]

{ #category : #initialization }
BalloonMorph >> initialize [
	"initialize the state of the receiver"
	
	balloonColor := self class balloonColor.
	super initialize.
	self beSmoothCurve.
	offsetFromTarget := 0@0.
]

{ #category : #'wiw support' }
BalloonMorph >> morphicLayerNumber [

	"helpful for insuring some morphs always appear in front of or behind others.
	smaller numbers are in front"

	^5		"Balloons are very front-like things"
]

{ #category : #initialization }
BalloonMorph >> popUpFor: aMorph hand: aHand [
	"Pop up the receiver as balloon help for the given hand"
	balloonOwner := aMorph.
	self popUpForHand: aHand.
]

{ #category : #initialization }
BalloonMorph >> popUpForHand: aHand [
	"Pop up the receiver as balloon help for the given hand"
	| worldBounds |

	self lock.
	self fullBounds. "force layout"
	self setProperty: #morphicLayerNumber toValue: self morphicLayerNumber.
	aHand world addMorphFront: self.
	"So that if the translation below makes it overlap the receiver, it won't
	interfere with the rootMorphsAt: logic and hence cause flashing.  Without
	this, flashing happens, believe me!"
	((worldBounds := aHand world bounds) containsRect: self bounds) ifFalse:
		[self bounds: (self bounds translatedToBeWithin: worldBounds)].
	aHand balloonHelp: self.

]

{ #category : #private }
BalloonMorph >> setTarget: aMorph [
	(target := aMorph) ifNotNil: [offsetFromTarget := self position - target position]
]

{ #category : #'stepping and presenter' }
BalloonMorph >> step [
	"Move with target."

	target ifNotNil: [self position: target position + offsetFromTarget].

]

{ #category : #testing }
BalloonMorph >> stepTime [
	^ 0  "every cycle"
]
