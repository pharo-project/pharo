"
I allow my images to be rescaled when a new total width is given. 

New instances should set the following:

- original: the original image later scaled
- scale: percentage - 1 to 100, not 0-1.

optionally one can freeze the height of the embedded image. This is mostly useful for morphs of text which should not scale in height

New width is given by targetWidth: - this is not a user function, but is called by the scaling mechanism.
"
Class {
	#name : #MicScalingTextAnchor,
	#superclass : #TextAnchor,
	#instVars : [
		'original',
		'scale',
		'freezeHeight'
	],
	#category : #'Microdown-RichTextComposer-Composer'
}

{ #category : #accessing }
MicScalingTextAnchor >> freezeHeight [

	^ freezeHeight
]

{ #category : #accessing }
MicScalingTextAnchor >> freezeHeight: anObject [

	freezeHeight := anObject
]

{ #category : #accessing }
MicScalingTextAnchor >> original [

	^ original
]

{ #category : #accessing }
MicScalingTextAnchor >> original: aForm [

	original := aForm.
	anchoredMorph := aForm
]

{ #category : #accessing }
MicScalingTextAnchor >> scale: percentOrNil [
	scale := percentOrNil 
]

{ #category : #accessing }
MicScalingTextAnchor >> targetWidth: rightMargin [

	| newWidth newHeight |
	newWidth := rightMargin / 100 * scale.
	newHeight := freezeHeight
		ifNil: [ original height / original width * newWidth ]
		ifNotNil: [freezeHeight ].
	anchoredMorph := original  isMorph
		ifTrue: [ original extent: newWidth @ newHeight ]
		ifFalse: [ original scaledToSize: newWidth @ newHeight ]
]
