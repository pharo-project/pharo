"
I am a abstract visitor for handling events.. 
serving just to cover the complete gesture protocol.
"
Class {
	#name : #OSWindowEventGestureVisitor,
	#superclass : #OSWindowEventVisitor,
	#category : #OSWindow-Core-Gestures
}

{ #category : #visiting }
OSWindowEventGestureVisitor >> handleGestureEvent: anEvent [
	self handleEvent: anEvent.
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchActionPointerDownEvent: anEvent [
	"This event is only send when using the gesture detector"
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchActionPointerMoveEvent: anEvent [
	"This event is only send when using the gesture detector"
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchActionPointerUpEvent: anEvent [
	"This event is only send when using the gesture detector"
]

{ #category : #visiting }
OSWindowEventGestureVisitor >> visitTouchOneFingerDoubleTapEvent: anEvent [
]

{ #category : #visiting }
OSWindowEventGestureVisitor >> visitTouchOneFingerHoldEvent: anEvent [
]

{ #category : #'visiting - three fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchThreeFingersScrollBeginEvent: anEvent [
]

{ #category : #'visiting - three fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchThreeFingersScrollEndEvent: anEvent [
]

{ #category : #'visiting - three fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchThreeFingersScrollMoveEvent: anEvent [
]

{ #category : #'visiting - three fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchThreeFingersSwipeAbortEvent: anEvent [
	
]

{ #category : #'visiting - three fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchThreeFingersSwipeBeginEvent: anEvent [
]

{ #category : #'visiting - three fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchThreeFingersSwipeEndEvent: anEvent [
	
]

{ #category : #'visiting - three fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchThreeFingersSwipeMoveEvent: anEvent [
	
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersPinchBeginEvent: anEvent [
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersPinchEndEvent: anEvent [
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersPinchMoveEvent: anEvent [
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersRotationBeginEvent: anEvent [
	
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersRotationEndEvent: anEvent [
	
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersRotationMoveEvent: anEvent [
	
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersScrollBeginEvent: anEvent [
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersScrollEndEvent: anEvent [
]

{ #category : #'visiting - two fingers gestures' }
OSWindowEventGestureVisitor >> visitTouchTwoFingersScrollMoveEvent: anEvent [
]
