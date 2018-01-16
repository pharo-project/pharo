"
Only one gesture at the time, you can scroll to move the circle, pinch to zoom, rotate to rotate, double tap to change the color of the circle and swipe to have a little surprise
"
Class {
	#name : #SDL2TouchGestureExample2,
	#superclass : #SDL2TouchExample,
	#instVars : [
		'circleSize',
		'negativeColor',
		'positiveColor',
		'circleCenter',
		'hasAbortedSwipe',
		'discoMode',
		'blackBackground',
		'blackBackgroundPosition'
	],
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #'OSWindow-SDL2-Examples-Gesture'
}

{ #category : #examples }
SDL2TouchGestureExample2 class >> example [
	^ self new open
]

{ #category : #'manage swipe' }
SDL2TouchGestureExample2 >> abortedSwipe [
	[ hasAbortedSwipe := true.
	(DelayWaitTimeout forSeconds: 2) wait.
	hasAbortedSwipe := false ] fork
]

{ #category : #'manage pinch' }
SDL2TouchGestureExample2 >> beginPinch: anEvent [
]

{ #category : #'manage rotation' }
SDL2TouchGestureExample2 >> beginRotation: anEvent [
]

{ #category : #'manage scrolling' }
SDL2TouchGestureExample2 >> beginScroll: anEvent [
]

{ #category : #'manage swipe' }
SDL2TouchGestureExample2 >> beginSwipe: anEvent [
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> changeColors [
	positiveColor := Color random.
	negativeColor := Color random
]

{ #category : #factory }
SDL2TouchGestureExample2 >> createEventHandler [
	^ OSWindowGestureEventHandler new
			touchExample: self;
			registerGesture: OSTouchTwoFingersScrollDetector new;
			registerGesture: OSTouchTwoFingersPinchDetector new;
			registerGesture: OSTouchThreeFingersSwipeDetector new;
			registerGesture: OSTouchTwoFingersRotationDetector new;
			registerGesture: OSTouchOneFingerDoubleTapDetector new;
			registerGesture: OSTouchOneFingerHoldDetector new;
			yourself
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> displayAbortMessage [
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas pathTransform translateBy: self windowCenter x - 30 @ self windowCenter y.
			canvas
				setPaint: Color green;
				setFont: ((LogicalFont familyName: 'Arial' pointSize: 1) emphasis: 1);
				setShape: (renderer athensSurface createPath: [ :builder | builder close ]);
				drawString: 'Swipe Aborted!';
				draw ]
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> drawBlackBackground [
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas setPaint: blackBackground.
			canvas drawShape: (blackBackgroundPosition @ 0 extent: window extent) ]
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> drawCircle [
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas pathTransform translateBy: circleCenter.
			canvas
				setPaint:
						(circleSize positive
								ifTrue: [ positiveColor ]
								ifFalse: [ negativeColor ]);
				setShape: (self generateCircleOfSize: circleSize);
				draw ]
]

{ #category : #'manage pinch' }
SDL2TouchGestureExample2 >> endPinch: anEvent [
]

{ #category : #'manage rotation' }
SDL2TouchGestureExample2 >> endRotation: anEvent [
]

{ #category : #'manage scrolling' }
SDL2TouchGestureExample2 >> endScroll: anEvent [
]

{ #category : #'manage swipe' }
SDL2TouchGestureExample2 >> endSwipe: anEvent [
	blackBackgroundPosition := blackBackgroundPosition abs
		> (window extent x / 2)
		ifTrue: [ 0 ]
		ifFalse: [ window extent x ]
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> generateCircleOfSize: anInteger [
	^ renderer athensSurface
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: anInteger negated @ 0;
				ccwArcTo: 0 @ anInteger angle: 90 degreesToRadians;
				ccwArcTo: anInteger @ 0 angle: 90 degreesToRadians;
				ccwArcTo: 0 @ anInteger negated angle: 90 degreesToRadians;
				ccwArcTo: anInteger negated @ 0 angle: 90 degreesToRadians ]
]

{ #category : #initialization }
SDL2TouchGestureExample2 >> initialize [
	super initialize.
	circleCenter := self windowCenter.
	circleSize := 10.
	negativeColor := Color blue.
	positiveColor := Color red.
	hasAbortedSwipe := false.
	blackBackgroundPosition := self defaultExtent x.
	discoMode := false
]

{ #category : #initialization }
SDL2TouchGestureExample2 >> initializeFromSurface [
	super initializeFromSurface.
	blackBackground := renderer athensSurface createSolidColorPaint: Color black
]

{ #category : #'manage pinch' }
SDL2TouchGestureExample2 >> pinch: anEvent [
	circleSize := circleSize + (anEvent dDist * (self defaultExtent distanceTo: 0@0))
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> randomizeCircleColor [
	positiveColor := Color random.
	negativeColor := Color random.
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> redraw [
	discoMode ifTrue: 
		[ self randomizeCircleColor ].
	self clearSurface.
	self drawBlackBackground.
	self drawMoves.
	self drawCircle.
	hasAbortedSwipe
		ifTrue: [ self displayAbortMessage ]
]

{ #category : #'manage rotation' }
SDL2TouchGestureExample2 >> rotation: anEvent [
	circleCenter := circleCenter 
		rotateBy: 2 * anEvent dTheta negated
		about: self windowCenter
]

{ #category : #'manage scrolling' }
SDL2TouchGestureExample2 >> scroll: anEvent [
	| newCenter |
	newCenter := circleCenter
		+ (anEvent direction abs * (anEvent dDist * (window extent distanceTo: 0 @ 0))).
	(newCenter < window extent and: [ newCenter > (0 @ 0) ])
		ifTrue: [ circleCenter := newCenter ]
]

{ #category : #'manage swipe' }
SDL2TouchGestureExample2 >> swipe: anEvent [
	blackBackgroundPosition := blackBackgroundPosition + (anEvent direction x * 1.5)
]

{ #category : #drawing }
SDL2TouchGestureExample2 >> switchDiscoMode [
	discoMode := discoMode not
]

{ #category : #initialization }
SDL2TouchGestureExample2 >> windowCenter [
	^ (self defaultExtent x / 2) asFloat @ (self defaultExtent y / 2) asFloat
]
