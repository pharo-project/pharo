"
I am a single frame in a sequence of frames that comprise an animated image. I am designed for use by classes like AnimatedImageMorph and am created during the reading of some image files such as GIFs (see GIFReadWriter).

I contain a Form describing my image, a delay time in milliseconds that describes how long I should be displayed in a sequence of animated frames, and a disposal symbol that implementors can use when compositing myself in a sequence of frames. 

See AnimatedImageMorph for examples of how I am used.
"
Class {
	#name : #AnimatedImageFrame,
	#superclass : #Object,
	#instVars : [
		'form',
		'delay',
		'disposal',
		'offset'
	],
	#category : #'Images-Animated'
}

{ #category : #defaults }
AnimatedImageFrame >> defaultDelay [
	^ 66
]

{ #category : #accessing }
AnimatedImageFrame >> delay [
	^ delay
]

{ #category : #accessing }
AnimatedImageFrame >> delay: aNumber [
	delay := aNumber
]

{ #category : #accessing }
AnimatedImageFrame >> disposal [
	^ disposal
]

{ #category : #accessing }
AnimatedImageFrame >> disposal: aSymbol [
	"Disposal must be one of:
		#restoreBackground
		#leaveCurrent
		#restorePreviousState"
	"({ #restoreBackground.
		#leaveCurrent.
		#restorePreviousState } includes: aSymbol) ifTrue: [ 
			disposal := aSymbol ]."
	disposal := aSymbol
]

{ #category : #accessing }
AnimatedImageFrame >> form [
	^ form
]

{ #category : #accessing }
AnimatedImageFrame >> form: aForm [
	form := aForm
]

{ #category : #initialization }
AnimatedImageFrame >> initialize [
	super initialize.
	offset := 0 @ 0.
	delay := self defaultDelay.
	disposal := #otherDisposal
]

{ #category : #accessing }
AnimatedImageFrame >> offset [
	^ offset
]

{ #category : #accessing }
AnimatedImageFrame >> offset: aPoint [
	"This represents the frame form's offset in the
	parent image canvas"
	offset := aPoint
]
