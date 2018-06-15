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

{ #category : #'as yet unclassified' }
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
	offset := 0@0.
	delay := self defaultDelay.
	form := nil.
	disposal := #otherDisposal.
	
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
