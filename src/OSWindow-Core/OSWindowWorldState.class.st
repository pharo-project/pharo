"
I am a world state that does not use the Display global variable.
"
Class {
	#name : #OSWindowWorldState,
	#superclass : #WorldState,
	#instVars : [
		'display'
	],
	#category : 'OSWindow-Core-Morphic'
}

{ #category : #'as yet unclassified' }
OSWindowWorldState >> assuredCanvas [

	(canvas isNil or: [(canvas extent ~= viewBox extent) or: [canvas form depth ~= Display depth]])
		ifTrue:
			["allocate a new offscreen canvas the size of the window"
			self canvas: (display defaultCanvasClass extent: viewBox extent)].
	^ self canvas
]

{ #category : #accessing }
OSWindowWorldState >> display [
	^ display
]

{ #category : #accessing }
OSWindowWorldState >> display: anObject [
	display := anObject
]

{ #category : #accessing }
OSWindowWorldState >> displayWorld: aWorld submorphs: submorphs [
	aWorld updateOnOSWindow
]

{ #category : #accessing }
OSWindowWorldState >> doDeferredUpdatingFor: aWorld [
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, do nothing and answer false. One can set the class variable DisableDeferredUpdates to true to completely disable the deferred updating feature."

	display ifNil: [ ^ false ].
	(display deferUpdates: true) ifNil: [^ false].  "deferred updates not supported"
	(canvas notNil and: [canvas form == display]) ifFalse: [
			aWorld viewBox: display boundingBox.    "do first since it may clear canvas"
			self canvas: (display getCanvas copyClipRect: display boundingBox)].
	^ true

]

{ #category : #'as yet unclassified' }
OSWindowWorldState >> doOneCycleNowFor: aWorld [
	"process user input events"
	LastCycleTime := Time millisecondClockValue.
	self handsDo: [:h |
		self activeHand: h.
		h processEventsFromQueue: nil.
		self activeHand: nil.
	].

	"the default is the primary hand"
	self activeHand: self hands first.

	aWorld runStepMethods.		"there are currently some variations here"
	self displayWorldSafely: aWorld.
]
