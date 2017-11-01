"
I am the handler for the gestures.
As you can see instead of using #visitTouchPointerUpEvent: anEvent you have to use #visitTouchActionPointerUpEvent: anEvent
"
Class {
	#name : #OSWindowGestureEventHandler,
	#superclass : #OSWindowGestureHandler,
	#instVars : [
		'touchExample'
	],
	#category : 'OSWindow-SDL2-Examples-Gesture'
}

{ #category : #api }
OSWindowGestureEventHandler >> touchExample: renderer [
	touchExample := renderer
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchActionPointerDownEvent: anEvent [
	touchExample beginMove: anEvent fingerId at: anEvent position
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchActionPointerMoveEvent: anEvent [
	touchExample move: anEvent fingerId at: anEvent position.
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchActionPointerUpEvent: anEvent [
	touchExample endMove: anEvent fingerId at: anEvent position.
]

{ #category : #visiting }
OSWindowGestureEventHandler >> visitTouchOneFingerDoubleTapEvent: anEvent [
	touchExample changeColors
]

{ #category : #visiting }
OSWindowGestureEventHandler >> visitTouchOneFingerHoldEvent: anEvent [
	touchExample switchDiscoMode
]

{ #category : #'visiting - three fingers gestures' }
OSWindowGestureEventHandler >> visitTouchThreeFingersSwipeAbortEvent: anEvent [
	touchExample abortedSwipe.
]

{ #category : #'visiting - three fingers gestures' }
OSWindowGestureEventHandler >> visitTouchThreeFingersSwipeBeginEvent: anEvent [
	touchExample beginSwipe: anEvent
]

{ #category : #'visiting - three fingers gestures' }
OSWindowGestureEventHandler >> visitTouchThreeFingersSwipeEndEvent: anEvent [
	touchExample endSwipe: anEvent
]

{ #category : #'visiting - three fingers gestures' }
OSWindowGestureEventHandler >> visitTouchThreeFingersSwipeMoveEvent: anEvent [
	touchExample swipe: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersPinchBeginEvent: anEvent [
	touchExample beginPinch: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersPinchEndEvent: anEvent [
	touchExample endPinch: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersPinchMoveEvent: anEvent [
	touchExample pinch: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersRotationBeginEvent: anEvent [
	touchExample beginRotation: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersRotationEndEvent: anEvent [
	touchExample endRotation: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersRotationMoveEvent: anEvent [
	touchExample rotation: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersScrollBeginEvent: anEvent [
	touchExample beginScroll: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersScrollEndEvent: anEvent [
	touchExample endScroll: anEvent
]

{ #category : #'visiting - two fingers gestures' }
OSWindowGestureEventHandler >> visitTouchTwoFingersScrollMoveEvent: anEvent [
	touchExample scroll: anEvent
]

{ #category : #visiting }
OSWindowGestureEventHandler >> visitWindowCloseEvent: anEvent [
	touchExample destroy
]
