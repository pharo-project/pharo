"
You can move the circle by scrolling and change his size by pinching
"
Class {
	#name : #SDL2TouchGestureExample,
	#superclass : #SDL2TouchExample,
	#instVars : [
		'circleCenter',
		'circleSize'
	],
	#category : #'OSWindow-SDL2-Examples-Gesture'
}

{ #category : #examples }
SDL2TouchGestureExample class >> example [
	^ self new open
]

{ #category : #'manage pinch' }
SDL2TouchGestureExample >> beginPinch: anEvent [
]

{ #category : #'manage scrolling' }
SDL2TouchGestureExample >> beginScroll: anEvent [
]

{ #category : #factory }
SDL2TouchGestureExample >> createEventHandler [
	^OSWindowGestureEventHandler new
				touchExample: self;
				registerGesture: OSTouchTwoFingersScrollDetector new;
				registerGesture: OSTouchTwoFingersPinchDetector new;
				yourself
]

{ #category : #drawing }
SDL2TouchGestureExample >> drawCircle [
	renderer athensSurface
		drawDuring: [ :canvas | 
			canvas pathTransform translateBy: circleCenter.
			canvas
				setPaint:
						(circleSize positive
								ifTrue: [ Color red ]
								ifFalse: [ Color blue ]);
				setShape: (self generateCircleOfSize: circleSize);
				draw ]
]

{ #category : #'manage pinch' }
SDL2TouchGestureExample >> endPinch: anEvent [
]

{ #category : #'manage scrolling' }
SDL2TouchGestureExample >> endScroll: anEvent [
]

{ #category : #drawing }
SDL2TouchGestureExample >> generateCircleOfSize: anInteger [
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
SDL2TouchGestureExample >> initialize [
	super initialize.
	circleCenter := (self defaultExtent x / 2) asFloat @ (self defaultExtent y / 2) asFloat.
	circleSize := 10.0
]

{ #category : #'manage pinch' }
SDL2TouchGestureExample >> pinch: anEvent [
	circleSize := circleSize + (anEvent dDist * (self defaultExtent distanceTo: 0@0)).
]

{ #category : #drawing }
SDL2TouchGestureExample >> redraw [
	super redraw.
	self drawCircle
]

{ #category : #'manage scrolling' }
SDL2TouchGestureExample >> scroll: anEvent [
	circleCenter := circleCenter
		+ (anEvent direction * (anEvent dDist abs * (self defaultExtent distanceTo: 0 @ 0)))
]
