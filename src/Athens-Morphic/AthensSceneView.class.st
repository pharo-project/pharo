"
i am simple morph for representing some scene.

The scene is any object which implements #renderOn: method,
or a block with single argument.
(an argument passed is an Athens canvas).

I implement a simple view panning and zooming with mouse drag and mouse-wheel (correspondigly).

Example1: open scene view, by passing a simple rendering block.

```
| view |
view  := AthensSceneView new.

view scene: [:canvas |
	canvas surface clear:  Color black.
	canvas setPaint: Color red.
	canvas drawShape: (0@0 corner:120@100)
].
view openInWindow.
```

Example2: open scene view on imported SVG file (note you need 'Athens-SVG' package loaded):

```
| view |
view  := AthensSceneView new.
view scene: (AthensSVGConverter fromFile: 'lion.svg').

view openInWindow.
```
"
Class {
	#name : #AthensSceneView,
	#superclass : #Morph,
	#instVars : [
		'surface',
		'scene',
		'session',
		'transform',
		'state',
		'keepRefreshing'
	],
	#category : #'Athens-Morphic'
}

{ #category : #colors }
AthensSceneView >> backgroundColor [
	^ Color gray
]

{ #category : #'session management' }
AthensSceneView >> checkSession [
	session == Smalltalk session ifFalse: [
		self initializeForNewSession ]
]

{ #category : #'session management' }
AthensSceneView >> createSurface [
	| extent |
	extent := self extent asIntegerPoint.
	surface := (scene respondsTo: #createSurface:)
		ifTrue: [ scene createSurface: extent ]
		ifFalse: [ AthensCairoSurface extent: extent]
]

{ #category : #drawing }
AthensSceneView >> drawOn: canvas [
	| px py |
	canvas fillRectangle: bounds color: self backgroundColor.
	self renderScene.

	surface displayOnMorphicCanvas: canvas at: bounds origin.

"	 translucentImage: surface asForm at: bounds origin."

	self showDebugInfo
		ifTrue: [
			px := transform x printShowingDecimalPlaces: 3.
			py := transform y printShowingDecimalPlaces: 3.
			canvas
				drawString: 'zoom: ' , (transform sx printShowingDecimalPlaces: 3) , ' pan: ' , px , ' @ ' , py
				at: bounds origin
				font: nil
				color: Color white ]
]

{ #category : #'event handling' }
AthensSceneView >> handlesMouseDown: evt [
	^ true
]

{ #category : #'event handling' }
AthensSceneView >> handlesMouseOver: evt [
	^ true
]

{ #category : #'event handling' }
AthensSceneView >> handlesMouseWheel: event [
	^ true
]

{ #category : #'state tracking' }
AthensSceneView >> inState: stateName [


	^ (state at: stateName ifAbsent: nil) notNil
]

{ #category : #'state tracking' }
AthensSceneView >> inState: stateName do: aBlock [


	^ (state at: stateName ifAbsent: nil) ifNotNil: aBlock
]

{ #category : #initialization }
AthensSceneView >> initialize [

	super initialize.
	keepRefreshing := false.
	transform := AthensAffineTransform new.

	self hResizing: #spaceFill.
	self vResizing: #spaceFill.
	color := Color transparent.
	state := Dictionary new.
	self extent: self minimumExtent
]

{ #category : #'session management' }
AthensSceneView >> initializeForNewSession [
	self createSurface.
	session := Smalltalk session
]

{ #category : #accessing }
AthensSceneView >> keepRefreshing: aBoolean [
	keepRefreshing := aBoolean.

	keepRefreshing ifTrue: [  self startStepping ]
]

{ #category : #layout }
AthensSceneView >> layoutChanged [
	"react on morph resize"
	super layoutChanged.
	surface ifNotNil: [
		self extent asIntegerPoint ~= surface extent ifTrue: [
			self createSurface ]
	]
]

{ #category : #initialization }
AthensSceneView >> minimumExtent [
	^ 300@300
]

{ #category : #'event handling' }
AthensSceneView >> mouseDown: evt [

	| pos |

	pos := evt cursorPoint.

	"left button"
	evt redButtonPressed ifTrue: [
		self setState: #panning value: { transform x@transform y. pos }
		 ]
]

{ #category : #'event handling' }
AthensSceneView >> mouseEnter: evt [

	self setState: #mouseIn
]

{ #category : #'event handling' }
AthensSceneView >> mouseLeave: evt [
	self
		resetState: #mouseIn;
		resetState: #panning
]

{ #category : #'event handling' }
AthensSceneView >> mouseMove: evt [

	self inState: #panning do: [ :startPanAndPos |
		| delta |
		delta := startPanAndPos first + ( evt cursorPoint - startPanAndPos second ).

		transform
			x: delta x;
			y: delta y.
		self changed.
	]
]

{ #category : #'event handling' }
AthensSceneView >> mouseUp: evt [

	"self halt."

	self resetState: #panning
]

{ #category : #'event handling' }
AthensSceneView >> mouseWheel: event [


	"Handle a mouseWheel event."

	| center zoom  |

	center := transform inverseTransform: (event cursorPoint - bounds origin).

	zoom := 1.

	event isUp ifTrue: [ zoom := 1.25 ].
	event isDown ifTrue: [ zoom := 1/1.25 ].


	(self inState: #zooming) ifTrue: [
		self updateZoom: zoom cursor: event cursorPoint.
	] ifFalse: [
		self startZooming: zoom center: center.
	]
]

{ #category : #drawing }
AthensSceneView >> renderScene [

	self checkSession.

	scene ifNotNil: [
		surface drawDuring: [ :acanvas |
			surface clear.
			acanvas pathTransform loadAffineTransform: transform.
			scene isBlock
				ifTrue: [ scene cull: acanvas cull: self ]
				ifFalse: [ scene renderOn: acanvas ] ] ]
]

{ #category : #'state tracking' }
AthensSceneView >> resetState: stateName [

	state at: stateName put: nil.

	"receiver's state changed"

	self changed
]

{ #category : #accessing }
AthensSceneView >> scene [
	^ scene
]

{ #category : #accessing }
AthensSceneView >> scene: aScene [

	scene := aScene.

	self changed
]

{ #category : #'state tracking' }
AthensSceneView >> setState: stateName [

	state at: stateName put: true.

	"receiver's state changed"

	self changed
]

{ #category : #'state tracking' }
AthensSceneView >> setState: stateName value: aValue [

	state at: stateName put: aValue.

	"receiver's state changed"

	self changed
]

{ #category : #drawing }
AthensSceneView >> showDebugInfo [
	^ true
]

{ #category : #'zoom animation' }
AthensSceneView >> startZooming: zoom center: center [
	| start end |


	start := Time millisecondClockValue.
	end := start + 250.

	self setState: #zooming value: {  center. transform copy. zoom. start. end}.

	self startStepping
]

{ #category : #stepping }
AthensSceneView >> step [

	self inState: #zooming do: [ :zoomState |
		| now start end center targetZoom factor |
		now := Time millisecondClockValue.
		start := zoomState at: 4.
		end := zoomState at: 5.

		(now between: start and:end) ifFalse: [ now := end.
			self resetState: #zooming.
			self stopStepping ].

		transform := (zoomState at: 2) copy.
		center := zoomState at: 1.

		factor :=  (now-start)/(end - start ).
		targetZoom := zoomState at: 3.
		targetZoom := 1* (1-factor) + (targetZoom * factor).

		transform
			translateBy: center;
			scaleBy:targetZoom;
			translateBy: center negated.

	].
	self changed
]

{ #category : #stepping }
AthensSceneView >> stepTime [
	^ 0
]

{ #category : #stepping }
AthensSceneView >> stopStepping [
	keepRefreshing ifFalse: [
			^ super stopStepping ]
]

{ #category : #'zoom animation' }
AthensSceneView >> updateZoom: zoom cursor: cursorPoint [

	| zoomState targetZoom start end now fraction newCenter |
	zoomState := state at:#zooming.

	"change the target zoom and increase time a bit"
	targetZoom := zoomState at: 3.

	start := zoomState at: 4.
	end := zoomState at: 5.

	now := Time millisecondClockValue.

	(now > end) ifTrue: [  now := end ].

	 "interpolate linearly the target zoom factor over time start ... end"
	fraction := (now - start) / (end - start).

	"zoom goes from 1..target zoom"
	targetZoom := 1*(1-fraction) + (targetZoom * fraction).

	self step. "to update transform"
	zoomState at: 3 put: targetZoom * zoom.
	newCenter :=   transform inverseTransform: (cursorPoint - bounds origin).
	zoomState at: 1 put: newCenter.
	zoomState at: 2 put: transform copy.
	zoomState at: 4 put: now.
	zoomState at: 5 put: now + 250
]
