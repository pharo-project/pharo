Class {
	#name : #AnimatedImageMorph,
	#superclass : #Morph,
	#instVars : [
		'frames',
		'frameStream',
		'currentFrame',
		'activeForm',
		'prevFrame',
		'currentDisposal'
	],
	#category : #'Images-Animated'
}

{ #category : #'instance creation' }
AnimatedImageMorph class >> fromGIFReader: aGIFReader [
	"Respond with an instance of the receiver
	configured from a GIFReader instance"
	^ self new
		frames: aGIFReader frames;
		extent: (aGIFReader canvasWidth)@(aGIFReader canvasHeight);
		color: aGIFReader backgroundColor
]

{ #category : #accessing }
AnimatedImageMorph >> currentFrame [
	^ currentFrame
]

{ #category : #'as yet unclassified' }
AnimatedImageMorph >> defaultDelay [
	^ 100
]

{ #category : #drawing }
AnimatedImageMorph >> drawOn: aCanvas [
	"currentFrame ifNil: [ ^ self ]."
	aCanvas
		translucentImage: activeForm
		at: self bounds origin.
]

{ #category : #accessing }
AnimatedImageMorph >> forms [
	^ frames collect: [ :frame | frame form ].
]

{ #category : #accessing }
AnimatedImageMorph >> frameStream [
	^ frameStream
]

{ #category : #accessing }
AnimatedImageMorph >> frames [
	^ frames
]

{ #category : #accessing }
AnimatedImageMorph >> frames: aCollection [
	"A collection of AnimagedImageFrame instances"
	frames := aCollection.
	frameStream := ReadStream on: frames.
]

{ #category : #initialization }
AnimatedImageMorph >> initialize [
	super initialize.
	self color: Color transparent.
	frames := OrderedCollection new.
	currentDisposal := #restoreBackground.
	activeForm := Form extent: self extent.
]

{ #category : #'as yet unclassified' }
AnimatedImageMorph >> leaveCurrent [
	"Update the activeForm using the #leaveCurrent
	disposal method"
	| canvas |
	canvas := activeForm getCanvas.
	canvas
		translucentImage: currentFrame form
		at: currentFrame offset.
	
	activeForm := canvas form.
]

{ #category : #accessing }
AnimatedImageMorph >> prevFrame [
	^ prevFrame
]

{ #category : #'as yet unclassified' }
AnimatedImageMorph >> restoreBackground [
	"Update the activeForm using the #restoreBackground
	disposal method"
	| canvas r |
	canvas := activeForm getCanvas.
	r := (prevFrame offset extent: (prevFrame form extent)).
	canvas
		"Restore the area of the prevFrame to the background color"
		fillRectangle: r
		color: self color;
		clipBy: r
		during: [ :c |
			c
				drawImage: currentFrame form
				at: currentFrame offset ].

	activeForm := canvas form.
]

{ #category : #'stepping and presenter' }
AnimatedImageMorph >> step [
	frameStream ifNil: [ ^ self ].
	frameStream atEnd ifTrue: [ 
		frameStream reset.
		prevFrame := nil.
		currentFrame := frameStream next.
		currentDisposal := #restoreBackground.
		self
			updateActiveForm
			changed.
		^ self ].
	(frameStream position = 0) ifTrue: [ 
		prevFrame := nil.
		currentDisposal := #restoreBackground  ]
	ifFalse: [ 
		prevFrame := frameStream peekBack.
		currentDisposal := prevFrame disposal ].
	currentFrame := frameStream next.
	self updateActiveForm.
	self changed.
]

{ #category : #'model - stepping' }
AnimatedImageMorph >> stepTime [
	frameStream ifNil: [ ^ self defaultDelay ].
	^ currentFrame delay.
]

{ #category : #'as yet unclassified' }
AnimatedImageMorph >> updateActiveForm [
	"Update the activeForm of the receiver based upon the
	disposal rules of the current and previous frames."
	| canvas |
	prevFrame isNil ifTrue: [ 
		canvas := FormCanvas extent: self bounds extent.
		canvas
			translucentImage: currentFrame form
			at: currentFrame offset.
		activeForm := canvas form.
		^ self ].
	prevFrame disposal = #leaveCurrent
		ifTrue: [ ^ self leaveCurrent ].
	"Otherwise we assume a restore background"
	^ self restoreBackground

]
