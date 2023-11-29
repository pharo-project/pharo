"
There is only one instance of me, Display. It is a global and is used to handle general user requests to deal with the whole display screen. 
	Although I offer no protocol, my name provides a way to distinguish this special instance from all other Forms. This is useful, for example, in dealing with saving and restoring the system.
	To change the depth of your Display...
		Display newDepth: 16.
		Display newDepth: 8.
		Display newDepth: 1.
Valid display depths are 1, 2, 4, 8, 16 and 32.  It is suggested that you run with your monitors setting the same, for better speed and color fidelity.  Note that this can add up to 4Mb for the Display form.  Finally, note that newDepth: ends by executing a 'ControlManager restore' which currently terminates the active process, so nothing that follows in the doit will get executed.

Depths 1, 2, 4 and 8 bits go through a color map to put color on the screen, but 16 and 32-bit color use the pixel values directly for RGB color (5 and 8 bits per, respectivlely).  The color choice an be observed by executing Color fromUser in whatever depth you are using.

"
Class {
	#name : 'DisplayScreen',
	#superclass : 'Form',
	#instVars : [
		'extraRegions'
	],
	#classVars : [
		'DeferringUpdates',
		'DisplayChangeSignature',
		'LastScreenModeSelected',
		'ScreenSave'
	],
	#category : 'Graphics-Display Objects-Screen',
	#package : 'Graphics-Display Objects',
	#tag : 'Screen'
}

{ #category : 'accessing' }
DisplayScreen class >> actualScreenDepth [

	^ Display depth
]

{ #category : 'accessing' }
DisplayScreen class >> actualScreenSize [
	"Delegates to active world because it can be something else than the one provided by the VM"

	self flag: #pharoTodo. "The method should be modified to use currentWorld, but the dependency is not possible. We have to remodularize it."
	^ self currentWorld ifNil: [ 0 @ 0 ] ifNotNil: #actualScreenSize
]

{ #category : 'display box access' }
DisplayScreen class >> boundingBox [
	"Answer the bounding box for the form representing the current display
	screen."

	^Display boundingBox
]

{ #category : 'deferring' }
DisplayScreen class >> deferUpdates: aBoolean [
	| wasDeferred |

	wasDeferred := DeferringUpdates == true.
	DeferringUpdates := aBoolean.
	^ wasDeferred
]

{ #category : 'settings' }
DisplayScreen class >> displaySettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #displayFullscreen)
		label: 'Fullscreen mode';
		parent: #desktopSettings;
		target: #Display;
		getSelector: #isFullscreen;
		setSelector: #fullscreen:;
		description: 'On platforms that support it, set fullscreen mode';
		default: false
]

{ #category : 'class initialization' }
DisplayScreen class >> initialize [

	| display |
	display := self new.
	display
		setExtent: 0@0
		depth: 32.
	Smalltalk globals at: #Display put: display.

	self deferUpdates: false
]

{ #category : 'displaying' }
DisplayScreen >> addExtraRegion: aRectangle for: regionDrawer [
	"Register the given rectangle as a region which is drawn by the specified region drawer. The region will be excluded from any updates when #forceDamageToScreen: is called. Note that the rectangle is only valid for a single update cycle; once #forceDamageToScreen: has been called, the region drawer and its region are being removed from the list"
	extraRegions ifNil:[extraRegions := #()].
	extraRegions := extraRegions copyWith: (Array with: regionDrawer with: aRectangle)
]

{ #category : 'deferring' }
DisplayScreen >> deferUpdates: aBoolean [
	^self class deferUpdates: aBoolean
]

{ #category : 'deferring' }
DisplayScreen >> deferUpdatesIn: aRectangle while: aBlock [
	| result |
	(self class deferUpdates: true) ifTrue: [^aBlock value].
	result := aBlock value.
	self class deferUpdates: false.
	^result
]

{ #category : 'other' }
DisplayScreen >> displayChangeSignature [

	^DisplayChangeSignature
]

{ #category : 'private' }
DisplayScreen >> findAnyDisplayDepth [
	"Return any display depth that is supported on this system."
	^self findAnyDisplayDepthIfNone:[
		"Ugh .... now this is a biggie - a system that does not support
		any of the display depths at all."
		Smalltalk
			logError:'Fatal error: This system has no support for any display depth at all.'
			inContext: thisContext.
		Smalltalk quitPrimitive. "There is no way to continue from here"
	]
]

{ #category : 'private' }
DisplayScreen >> findAnyDisplayDepthIfNone: aBlock [
	"Return any display depth that is supported on this system.
	If there is none, evaluate aBlock.
	First check higher values to avoid depth 1 to be always returned."
	^ #(32 16 8 4 2 1 -32 -16 -8 -4 -2 -1)
		detect:[ :bpp| self supportsDisplayDepth: bpp ]
		ifNone: [ aBlock value ]
]

{ #category : 'displaying' }
DisplayScreen >> flash: aRectangle [
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle
]

{ #category : 'displaying' }
DisplayScreen >> flash: aRectangle andWait: msecs [
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.

	(Delay forMilliseconds: msecs) wait.
	self reverse: aRectangle.

	(Delay forMilliseconds: msecs) wait
]

{ #category : 'displaying' }
DisplayScreen >> flashAll: rectangleList andWait: msecs [
	"Flash the areas of the screen defined by the given rectangles."

	rectangleList do: [:aRectangle | self reverse: aRectangle].

	(Delay forMilliseconds: msecs) wait.
	rectangleList do: [:aRectangle | self reverse: aRectangle].

	(Delay forMilliseconds: msecs) wait
]

{ #category : 'other' }
DisplayScreen >> fullBoundingBox [
	^ super boundingBox
]

{ #category : 'other' }
DisplayScreen >> fullscreen [
	"Display fullscreen"

	ScreenSave ifNil: [ ^ self ].
	Display := ScreenSave
]

{ #category : 'screen managing' }
DisplayScreen >> fullscreen: aBoolean [

	self class codeSupportAnnouncer announce: (FullscreenAnnouncement new
			 displayScreen: self;
			 fullscreen: (LastScreenModeSelected := aBoolean))
]

{ #category : 'other' }
DisplayScreen >> fullscreenMode: aBoolean [
	"On platforms that support it, set fullscreen mode to the value of the argument. (Note: you'll need to restore the Display after calling this primitive."
	"Display fullscreenMode: true. Display newDepth: Display depth"

	<primitive: 233>
	self primitiveFailed
]

{ #category : 'screen managing' }
DisplayScreen >> fullscreenOff [

	self fullscreen: false
]

{ #category : 'screen managing' }
DisplayScreen >> fullscreenOn [

	self fullscreen: true
]

{ #category : 'other' }
DisplayScreen >> height [
	^ self boundingBox height
]

{ #category : 'testing' }
DisplayScreen >> isDisplayScreen [
	^true
]

{ #category : 'screen managing' }
DisplayScreen >> isFullscreen [
	^ self lastScreenModeSelected
]

{ #category : 'screen managing' }
DisplayScreen >> lastScreenModeSelected [
	^ LastScreenModeSelected
		ifNil: [LastScreenModeSelected := false]
]

{ #category : 'other' }
DisplayScreen >> newDepth: pixelSize [
	"
	Display newDepth: 8.
	Display newDepth: 1
	"
	(self supportsDisplayDepth: pixelSize)
		ifFalse: [^ self inform: 'Display depth ' , pixelSize printString , ' is not supported on this system'].
	self newDepthNoRestore: pixelSize.
	self restore
]

{ #category : 'private' }
DisplayScreen >> newDepthNoRestore: pixelSize [

	UIManager default newDisplayDepthNoRestore: pixelSize
]

{ #category : 'other' }
DisplayScreen >> replacedBy: aForm do: aBlock [
	"Permits normal display to draw on aForm instead of the display."

	ScreenSave := self.
	Display := aForm.
	aBlock value.
	Display := self.
	ScreenSave := nil
]

{ #category : 'other' }
DisplayScreen >> restore [

	UIManager default restoreDisplay
]

{ #category : 'other' }
DisplayScreen >> restoreAfter: aBlock [
	"Evaluate the block, wait for a mouse click, and then restore the screen."

	UIManager default restoreDisplayAfter: aBlock
]

{ #category : 'private' }
DisplayScreen >> setExtent: aPoint depth: bitsPerPixel [  "DisplayScreen startUp"
	"This method is critical.  If the setExtent fails, there will be no
	proper display on which to show the error condition..."
	"ar 5/1/1999: ... and that is exactly why we check for the available display depths first."

	"RAA 27 Nov 99 - if depth and extent are the same and acceptable, why go through this.
	also - record when we change so worlds can tell if it is time to repaint"

	(depth == bitsPerPixel
		and: [ aPoint = self extent
		and: [ self supportsDisplayDepth: bitsPerPixel ] ])
		ifTrue: [ ^ self ].

	bits := nil.  "Free up old bitmap in case space is low"
	DisplayChangeSignature := (DisplayChangeSignature ifNil: [ 0 ]) + 1.
	(self supportsDisplayDepth: bitsPerPixel)
		ifTrue:[
			super setExtent: aPoint depth: bitsPerPixel ]
		ifFalse:[
			(self supportsDisplayDepth: bitsPerPixel negated)
				ifTrue:[
					super setExtent: aPoint depth: bitsPerPixel negated ]
				ifFalse:[
					"Search for a suitable depth"
					super setExtent: aPoint depth: self findAnyDisplayDepth ] ]
]

{ #category : 'initialization' }
DisplayScreen >> shutDown [
	"Minimize Display memory saved in image"
	self setExtent: 240@120 depth: depth
]

{ #category : 'other' }
DisplayScreen >> supportedDisplayDepths [
	"Return all pixel depths supported on the current host platform."
	^#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) select: [:d | self supportsDisplayDepth: d]
]

{ #category : 'other' }
DisplayScreen >> supportsDisplayDepth: pixelDepth [
	"Return true if this pixel depth is supported on the current host platform.
	Primitive. Optional."
	^#(1 2 4 8 16 32) includes: pixelDepth
]

{ #category : 'screen managing' }
DisplayScreen >> toggleFullscreen [
	self fullscreen: self isFullscreen not
]

{ #category : 'other' }
DisplayScreen >> usableArea [
	"Answer the usable area of the receiver."

	^ self boundingBox deepCopy
]

{ #category : 'other' }
DisplayScreen >> width [
	^ self boundingBox width
]
