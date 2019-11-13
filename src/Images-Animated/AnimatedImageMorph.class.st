"
I am a Morph that displays a sequence of Forms order, each as represented by AnimatedImageFrame objects.

To set or access the AnimatedImageFrame objects that I should display, use #frames and #frames: with a collection of frames.

For convenience, the #forms message will respond with a collection of the Forms inside of each AnimatedImageFrame.

Unlike ImageMorph (from whom I do not subclass), my extent must be explicitly set since each frame's form can have different dimensions and offsets.
"
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
AnimatedImageMorph >> backgroundColor [
	^ self color
]

{ #category : #accessing }
AnimatedImageMorph >> backgroundColor: aColor [
	self color: aColor.
]

{ #category : #accessing }
AnimatedImageMorph >> currentFrame [
	^ currentFrame
]

{ #category : #defaults }
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

{ #category : #drawing }
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

{ #category : #drawing }
AnimatedImageMorph >> restoreBackground [
	"Update the activeForm using the #restoreBackground
	disposal method"
	| canvas r |
	canvas := activeForm getCanvas.
	r := (prevFrame offset extent: (prevFrame form extent)).
	
	"We restore the area of the previous frame to the backgroundColor.
	 Usually this is transparent"
	canvas
		clipBy: r
		during: [ :c |
			c fillColor: self backgroundColor ].
	
	"Draw the current frame's form atop the current canvas form"
	canvas
		translucentImage: currentFrame form
		at: currentFrame offset.

	activeForm := canvas form.
]

{ #category : #stepping }
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

{ #category : #stepping }
AnimatedImageMorph >> stepTime [
	frameStream ifNil: [ ^ self defaultDelay ].
	^ currentFrame delay.
]

{ #category : #drawing }
AnimatedImageMorph >> updateActiveForm [

	"Update the activeForm of the receiver based upon the
	disposal rules of the current and previous frames."

	| canvas |

	prevFrame
		ifNil: [ canvas := FormCanvas extent: self bounds extent.
			canvas translucentImage: currentFrame form at: currentFrame offset.
			activeForm := canvas form.
			^ self
			].
	prevFrame disposal = #leaveCurrent
		ifTrue: [ ^ self leaveCurrent ].	"Otherwise we assume a restore background"
	^ self restoreBackground
]
