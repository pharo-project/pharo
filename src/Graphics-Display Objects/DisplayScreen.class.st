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
	#name : #DisplayScreen,
	#superclass : #Form,
	#instVars : [
		'clippingBox',
		'extraRegions'
	],
	#classVars : [
		'DeferringUpdates',
		'DisplayChangeSignature',
		'LastScreenModeSelected',
		'ScreenSave',
		'Title'
	],
	#category : #'Graphics-Display Objects'
}

{ #category : #accessing }
DisplayScreen class >> actualScreenDepth [
	<primitive: 'primitiveScreenDepth'>
	^ Display depth
]

{ #category : #accessing }
DisplayScreen class >> actualScreenSize [
	<primitive: 106>

	^ World osWindow 
		ifNotNil: [:anOSWindow | anOSWindow extent]
		ifNil: [ self primitiveFailed ].

]

{ #category : #'display box access' }
DisplayScreen class >> boundingBox [
	"Answer the bounding box for the form representing the current display 
	screen."

	^Display boundingBox
]

{ #category : #'display box access' }
DisplayScreen class >> checkForNewScreenSize [
	"Check whether the screen size has changed and if so take appropriate actions"

	UIManager default checkForNewDisplaySize
]

{ #category : #deferring }
DisplayScreen class >> deferUpdates: aBoolean [
	| wasDeferred |
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer whether updates were deferred before if the primitive succeeds, nil if it fails."

	wasDeferred := DeferringUpdates == true.
	DeferringUpdates := aBoolean.
	^(self primitiveDeferUpdates: aBoolean) ifNotNil: [wasDeferred]
]

{ #category : #'display box access' }
DisplayScreen class >> depth: depthInteger width: widthInteger height: heightInteger fullscreen: aBoolean [
	"Force Pharo's window (if there's one) into a new size and depth."
	"DisplayScreen depth: 8 width: 1024 height: 768 fullscreen: false"

	<primitive: 92>
	self primitiveFailed 
]

{ #category : #settings }
DisplayScreen class >> displaySettingsOn: aBuilder [ 
	<systemsettings>
	(aBuilder setting: #displayFullscreen)
		label: 'Fullscreen mode';
		parent: #desktopSettings;
		target: #Display;
		getSelector: #isFullscreen;
		setSelector: #fullscreen:;
		description: 'On platforms that support it, set fullscreen mode';
		default: false.

]

{ #category : #'host window access' }
DisplayScreen class >> hostWindowIndex [
	^ 1
]

{ #category : #'host window access' }
DisplayScreen class >> hostWindowSize: aPoint [ 
	self primitiveWindowSize: self hostWindowIndex width: aPoint x heigth: aPoint y

]

{ #category : #'host window access' }
DisplayScreen class >> hostWindowTitle: aString [ 
	self 
		primitiveWindowTitle: self hostWindowIndex 
		string: (UTF8TextConverter default convertFromSystemString: aString)
]

{ #category : #'initialize-release' }
DisplayScreen class >> initialize [
	| display |
	display := self new.
	display
		setExtent: self actualScreenSize
		depth: 32. 
	Smalltalk globals at: #Display put: display.
	display beDisplay.
	
	self deferUpdates: false.
	SessionManager default 
		registerGuiClassNamed: self name
		atPriority: 10.
]

{ #category : #deferring }
DisplayScreen class >> primitiveDeferUpdates: aBoolean [
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer the receiver if the primitive succeeds, nil if it fails."

	<primitive: 126>
	^ nil  "answer nil if primitive fails"
]

{ #category : #'host window access' }
DisplayScreen class >> primitiveWindowSize: id width: width heigth: height [ 
	<primitive: 'primitiveHostWindowSizeSet' module: 'HostWindowPlugin'>
	"ignore failure"
]

{ #category : #'host window access' }
DisplayScreen class >> primitiveWindowTitle: id string: titleString [ 
	<primitive: 'primitiveHostWindowTitle' module:'HostWindowPlugin'>
	"ignore failure"
]

{ #category : #'host window access' }
DisplayScreen class >> refreshHostWindowTitle [
	Title ifNotNil: [ self hostWindowTitle: Title ]
]

{ #category : #'host window access' }
DisplayScreen class >> setWindowTitle: aTitle [
	Title := aTitle.
	self refreshHostWindowTitle
]

{ #category : #'system startup' }
DisplayScreen class >> shutDown [ 
	"Minimize Display memory saved in image"
	Display shutDown.
]

{ #category : #'system startup' }
DisplayScreen class >> startUp [
	"DisplayScreen startUp"

	Display setExtent: self actualScreenSize depth: Display nativeDepth.
	Display beDisplay.
	self refreshHostWindowTitle
]

{ #category : #displaying }
DisplayScreen >> addExtraRegion: aRectangle for: regionDrawer [
	"Register the given rectangle as a region which is drawn by the specified region drawer. The region will be excluded from any updates when #forceDamageToScreen: is called. Note that the rectangle is only valid for a single update cycle; once #forceDamageToScreen: has been called, the region drawer and its region are being removed from the list"
	extraRegions ifNil:[extraRegions := #()].
	extraRegions := extraRegions copyWith: (Array with: regionDrawer with: aRectangle).

]

{ #category : #private }
DisplayScreen >> beDisplay [
	"Primitive. Tell the interpreter to use the receiver as the current display 
	image. Fail if the form is too wide to fit on the physical display. 
	Essential. See Object documentation whatIsAPrimitive."

	<primitive: 102>
	self primitiveFailed
]

{ #category : #other }
DisplayScreen >> boundingBox [
	clippingBox == nil
		ifTrue: [clippingBox := super boundingBox].
	^ clippingBox
]

{ #category : #other }
DisplayScreen >> clippingTo: aRect do: aBlock [ 
	"Display clippingTo: Rectangle fromUser do:"
	| saveClip |
	saveClip := clippingBox.
	clippingBox := aRect.
	aBlock value.
	clippingBox := saveClip
]

{ #category : #displaying }
DisplayScreen >> copyBits: rect from: sf at: destOrigin clippingBox: clipRect rule: cr fillColor: hf [ 
	(BitBlt
		destForm: self
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: rect origin
		extent: rect extent
		clipRect: (clipRect intersect: clippingBox ifNone: [ ^ self ])) copyBits
]

{ #category : #displaying }
DisplayScreen >> copyBits: rect from: sf at: destOrigin clippingBox: clipRect rule: cr fillColor: hf map: map [
	((BitBlt
		destForm: self
		sourceForm: sf
		fillColor: hf
		combinationRule: cr
		destOrigin: destOrigin
		sourceOrigin: rect origin
		extent: rect extent
		clipRect: (clipRect intersect: clippingBox ifNone: [ ^ self ] )) colorMap: map) copyBits
]

{ #category : #private }
DisplayScreen >> copyFrom: aForm [
	"Take on all state of aForm, with complete sharing"

	super copyFrom: aForm.
	clippingBox := super boundingBox
]

{ #category : #deferring }
DisplayScreen >> deferUpdates: aBoolean [
	^self class deferUpdates: aBoolean
]

{ #category : #deferring }
DisplayScreen >> deferUpdatesIn: aRectangle while: aBlock [
	| result |
	(self class deferUpdates: true) ifTrue: [^aBlock value].
	result := aBlock value.
	self class deferUpdates: false.
	self forceToScreen: aRectangle.
	^result
]

{ #category : #other }
DisplayScreen >> displayChangeSignature [

	^DisplayChangeSignature
]

{ #category : #private }
DisplayScreen >> findAnyDisplayDepth [
	"Return any display depth that is supported on this system."
	^self findAnyDisplayDepthIfNone:[
		"Ugh .... now this is a biggie - a system that does not support
		any of the display depths at all."
		Smalltalk
			logError:'Fatal error: This system has no support for any display depth at all.'
			inContext: thisContext.
		Smalltalk quitPrimitive. "There is no way to continue from here"
	].
]

{ #category : #private }
DisplayScreen >> findAnyDisplayDepthIfNone: aBlock [
	"Return any display depth that is supported on this system.
	If there is none, evaluate aBlock.
	First check higher values to avoid depth 1 to be always returned."
	^ #(32 16 8 4 2 1 -32 -16 -8 -4 -2 -1) 
		detect:[ :bpp| self supportsDisplayDepth: bpp ]
		ifNone: [ aBlock value ]
]

{ #category : #displaying }
DisplayScreen >> flash: aRectangle [ 
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceDisplayUpdate.
	(Delay forMilliseconds: 100) wait.
	self reverse: aRectangle.
	self forceDisplayUpdate.

]

{ #category : #displaying }
DisplayScreen >> flash: aRectangle andWait: msecs [
	"Flash the area of the screen defined by the given rectangle."

	self reverse: aRectangle.
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.
	self reverse: aRectangle.
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.

]

{ #category : #displaying }
DisplayScreen >> flashAll: rectangleList andWait: msecs [
	"Flash the areas of the screen defined by the given rectangles."

	rectangleList do: [:aRectangle | self reverse: aRectangle].
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.
	rectangleList do: [:aRectangle | self reverse: aRectangle].
	self forceDisplayUpdate.
	(Delay forMilliseconds: msecs) wait.

]

{ #category : #displaying }
DisplayScreen >> forceDamageToScreen: allDamage [
	"Force all the damage rects to the screen."
	| regions rectList |
	rectList := allDamage.
	"Note: Reset extra regions at the beginning to prevent repeated errors"
	regions := extraRegions.
	extraRegions := nil.
	regions ifNotNil:[
		"exclude extra regions"
		regions do:[:drawerAndRect| | excluded remaining |
			excluded := drawerAndRect at: 2.
			remaining := Array new writeStream.
			rectList do:[:r|
				remaining nextPutAll:(r areasOutside: excluded)].
			rectList := remaining contents].
	].
	rectList do:[:r| self forceToScreen: r].
	regions ifNotNil:[
		"Have the drawers paint what is needed"
		regions do:[:drawerAndRect| (drawerAndRect at: 1) forceToScreen].
	].
]

{ #category : #other }
DisplayScreen >> forceDisplayUpdate [
	"On platforms that buffer screen updates, force the screen to be updated immediately. On other platforms, or if the primitive is not implemented, do nothing."

	<primitive: 231>
	"do nothing if primitive fails"
]

{ #category : #other }
DisplayScreen >> forceToScreen [
	"Force the entire display area to the screen"
	^self forceToScreen: self boundingBox
]

{ #category : #other }
DisplayScreen >> forceToScreen: aRectangle [
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Typically used when the deferUpdates flag in the virtual machine is on; see deferUpdates:."

	self primShowRectLeft: aRectangle left
		right: aRectangle right
		top: aRectangle top
		bottom: aRectangle bottom.

]

{ #category : #other }
DisplayScreen >> fullBoundingBox [
	^ super boundingBox
]

{ #category : #other }
DisplayScreen >> fullscreen [
	"Display fullscreen"

	ScreenSave notNil ifTrue: [Display := ScreenSave].
	clippingBox := super boundingBox
]

{ #category : #'screen managing' }
DisplayScreen >> fullscreen: aBoolean [

	Display fullscreenMode: (LastScreenModeSelected := aBoolean).
	DisplayScreen checkForNewScreenSize.
	
]

{ #category : #other }
DisplayScreen >> fullscreenMode: aBoolean [
	"On platforms that support it, set fullscreen mode to the value of the argument. (Note: you'll need to restore the Display after calling this primitive."
	"Display fullscreenMode: true. Display newDepth: Display depth"

	<primitive: 233>
	self primitiveFailed

]

{ #category : #'screen managing' }
DisplayScreen >> fullscreenOff [

	self fullscreen: false
]

{ #category : #'screen managing' }
DisplayScreen >> fullscreenOn [

	self fullscreen: true
]

{ #category : #other }
DisplayScreen >> height [
	^ self boundingBox height
]

{ #category : #testing }
DisplayScreen >> isDisplayScreen [
	^true
]

{ #category : #'screen managing' }
DisplayScreen >> isFullscreen [
	^ self lastScreenModeSelected.
]

{ #category : #'screen managing' }
DisplayScreen >> lastScreenModeSelected [
	^ LastScreenModeSelected
		ifNil: [LastScreenModeSelected := false]
]

{ #category : #other }
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

{ #category : #private }
DisplayScreen >> newDepthNoRestore: pixelSize [
	
	UIManager default newDisplayDepthNoRestore: pixelSize
]

{ #category : #private }
DisplayScreen >> primRetryShowRectLeft: l right: r top: t bottom: b [
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. Do nothing if it fails. "

	<primitive: 127>
	"do nothing if primitive fails"

]

{ #category : #private }
DisplayScreen >> primShowRectLeft: l right: r top: t bottom: b [
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. If this fails, retry integer coordinates."

	<primitive: 127>
	"if this fails, coerce coordinates to integers and try again"
	self primRetryShowRectLeft: l truncated
		right: r rounded
		top: t truncated
		bottom: b rounded.

]

{ #category : #other }
DisplayScreen >> primitiveDeferUpdates: aBoolean [
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer the receiver if the primitive succeeds, nil if it fails."

	<primitive: 126>
	^ nil  "answer nil if primitive fails"

]

{ #category : #other }
DisplayScreen >> replacedBy: aForm do: aBlock [
	"Permits normal display to draw on aForm instead of the display."

	ScreenSave := self.
	Display := aForm.
	aBlock value.
	Display := self.
	ScreenSave := nil.
]

{ #category : #other }
DisplayScreen >> restore [
	
	UIManager default restoreDisplay
]

{ #category : #other }
DisplayScreen >> restoreAfter: aBlock [
	"Evaluate the block, wait for a mouse click, and then restore the screen."

	UIManager default restoreDisplayAfter: aBlock
]

{ #category : #private }
DisplayScreen >> setExtent: aPoint depth: bitsPerPixel [  "DisplayScreen startUp"
	"This method is critical.  If the setExtent fails, there will be no
	proper display on which to show the error condition..."
	"ar 5/1/1999: ... and that is exactly why we check for the available display depths first."

	"RAA 27 Nov 99 - if depth and extent are the same and acceptable, why go through this.
	also - record when we change so worlds can tell if it is time to repaint"

	(depth == bitsPerPixel and: [aPoint = self extent and: 
					[self supportsDisplayDepth: bitsPerPixel]]) ifFalse: [
		bits := nil.  "Free up old bitmap in case space is low"
		DisplayChangeSignature := (DisplayChangeSignature ifNil: [0]) + 1.
		(self supportsDisplayDepth: bitsPerPixel)
			ifTrue:[super setExtent: aPoint depth: bitsPerPixel]
			ifFalse:[(self supportsDisplayDepth: bitsPerPixel negated)
				ifTrue:[super setExtent: aPoint depth: bitsPerPixel negated]
				ifFalse:["Search for a suitable depth"
					super setExtent: aPoint depth: self findAnyDisplayDepth]].
	].
	clippingBox := super boundingBox
]

{ #category : #initialization }
DisplayScreen >> shutDown [ 
	"Minimize Display memory saved in image"
	self setExtent: 240@120 depth: depth
]

{ #category : #other }
DisplayScreen >> supportedDisplayDepths [
	"Return all pixel depths supported on the current host platform."
	^#(1 2 4 8 16 32 -1 -2 -4 -8 -16 -32) select: [:d | self supportsDisplayDepth: d]
]

{ #category : #other }
DisplayScreen >> supportsDisplayDepth: pixelDepth [
	"Return true if this pixel depth is supported on the current host platform.
	Primitive. Optional."
	<primitive: 91>
	^#(1 2 4 8 16 32) includes: pixelDepth
]

{ #category : #'screen managing' }
DisplayScreen >> toggleFullscreen [
	self fullscreen: self isFullscreen not
]

{ #category : #other }
DisplayScreen >> usableArea [
	"Answer the usable area of the receiver."

	^ self boundingBox deepCopy
]

{ #category : #other }
DisplayScreen >> width [
	^ self boundingBox width
]
