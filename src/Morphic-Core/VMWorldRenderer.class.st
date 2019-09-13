"
I know how to render the world using the VM backend.
"
Class {
	#name : #VMWorldRenderer,
	#superclass : #AbstractWorldRenderer,
	#instVars : [
		'display',
		'canvas'
	],
	#category : #'Morphic-Core-Worlds'
}

{ #category : #accessing }
VMWorldRenderer class >> isApplicableFor: aWorld [

    ^ Smalltalk isHeadless not
]

{ #category : #accessing }
VMWorldRenderer class >> priority [ 

	^ 1
]

{ #category : #operations }
VMWorldRenderer >> activateCursor: aCursor withMask: maskForm [

	aCursor beCursorWithMask: maskForm

]

{ #category : #accessing }
VMWorldRenderer >> actualScreenSize [

	<primitive: 106>

	self primitiveFailed
]

{ #category : #'display box access' }
VMWorldRenderer >> assuredCanvas [
	(self canvas isNil
		or: [ self canvas extent ~= self viewBox extent
				or: [ self canvas form depth ~= Display depth ] ])
		ifTrue:
			[ "allocate a new offscreen canvas the size of the window" 
				self canvas: (Display defaultCanvasClass extent: self viewBox extent) ].
	^ self canvas
]

{ #category : #accessing }
VMWorldRenderer >> canvas [

	^ canvas
]

{ #category : #accessing }
VMWorldRenderer >> canvas: x [ 
	canvas := x
]

{ #category : #'display box access' }
VMWorldRenderer >> checkForNewScreenSize [

	"Check whether the screen size has changed and if so take appropriate actions"

	Display extent = DisplayScreen actualScreenSize ifTrue: [^ Display].

   Display setExtent: self actualScreenSize depth: 32.
   Display beDisplay.

	world worldState realWindowExtent: self actualScreenSize.

	world restoreMorphicDisplay.

]

{ #category : #activation }
VMWorldRenderer >> deactivate [

	InputEventSensor default shutDown.
	
	Display shutDown.

	InputEventFetcher default shutDown.

]

{ #category : #operations }
VMWorldRenderer >> deferUpdates: aValue [

	^ Display deferUpdates: aValue
]

{ #category : #operations }
VMWorldRenderer >> displayWorldState: aWorldState ofWorld: aWorld submorphs: submorphs [
	"Update this world's display."

	| deferredUpdateMode handsToDraw allDamage |
	submorphs do: [ :m | m fullBounds ].	"force re-layout if needed"

	aWorldState checkIfUpdateNeeded
		ifFalse: [ ^ self ].	"display is already up-to-date"

	deferredUpdateMode := self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode
		ifFalse: [ self assuredCanvas ].

	self canvas
		ifNotNil: [ 
			self canvas
				roundCornersOf: aWorld
				during: [ | worldDamageRects handDamageRects |
					worldDamageRects := aWorldState drawWorld: aWorld submorphs: submorphs invalidAreasOn: self canvas.	"repair world's damage on canvas"
					"self handsDo:[:h| h noticeDamageRects: worldDamageRects]."
					handsToDraw := aWorldState selectHandsToDrawForDamage: worldDamageRects.
					handDamageRects := handsToDraw collect: [ :h | h savePatchFrom: self canvas ].
					allDamage := worldDamageRects , handDamageRects.
					handsToDraw reverseDo: [ :h | "draw hands onto world canvas" self canvas fullDrawMorph: h ] ] ].
	"*make this true to flash damaged areas for testing*"

	aWorldState class debugShowDamage
		ifTrue: [ aWorld flashRects: allDamage color: Color black ].

	"Check that the canvas is not already freed when we want to finish it"
	self canvas ifNotNil: [ :c | c finish ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [ self forceDamageToScreen: allDamage ]
		ifFalse: [ self canvas showAt: aWorld viewBox origin invalidRects: allDamage ].

	handsToDraw do: [ :h | h restoreSavedPatchOn: self canvas ].	"restore world canvas under hands"
	
	self
		deferUpdates: false;
		forceDisplayUpdate
]

{ #category : #activation }
VMWorldRenderer >> doActivate [

    InputEventFetcher default startUp.
    InputEventSensor installMouseDecodeTable.
    InputEventSensor default startUp.
    
    Display setExtent: self actualScreenSize depth: 32.
    Display beDisplay.

    canvas := nil.
    display := nil.

    self assuredCanvas.
    DisplayScreen refreshHostWindowTitle.
    Display forceDisplayUpdate.
    world displayWorld.
]

{ #category : #operations }
VMWorldRenderer >> doDeferredUpdatingFor: aWorld [
"If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, do nothing and answer false. One can set the class variable DisableDeferredUpdates to true to completely disable the deferred updating feature."

	(Display deferUpdates: true) ifNil: [^ false].  "deferred updates not supported"

	(self canvas notNil and: [self canvas form == Display]) ifFalse: [
			aWorld viewBox: self viewBox.    "do first since it may clear canvas"
			self canvas: (Display getCanvas copyClipRect: Display boundingBox)].

	^ true
]

{ #category : #operations }
VMWorldRenderer >> forceDamageToScreen: allDamage [

	^ Display forceDamageToScreen: allDamage.
	
]

{ #category : #operations }
VMWorldRenderer >> forceDisplayUpdate [

	^ Display forceDisplayUpdate
]

{ #category : #operations }
VMWorldRenderer >> fullscreenMode: aValue [

	Display fullscreenMode: aValue.
	self checkForNewScreenSize.
]

{ #category : #accessing }
VMWorldRenderer >> icon: aForm [ 

	"Do nothing.
	This is not supported anymore with the old VM world renderers.
	Use the new headless + SDL instead.
	
	Alternatively, if you're on windows you can try to use the following primitive from DisplayScreen that receives a path to a .ico file.
	This primitive only works on Windows.
	
	DisplayScreen hostWindowIcon: path.
	"
]

{ #category : #'display box access' }
VMWorldRenderer >> usableArea [

	^ Display usableArea
]

{ #category : #'display box access' }
VMWorldRenderer >> viewBox [ 

	^ Display boundingBox
]
