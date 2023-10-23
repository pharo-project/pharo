"
I know how to render the world in an OSWindow
"
Class {
	#name : 'OSWorldRenderer',
	#superclass : 'AbstractWorldRenderer',
	#instVars : [
		'driver',
		'osWindow',
		'display',
		'windowCloseAction',
		'previousFrameRenderingTime'
	],
	#category : 'OSWindow-Core-Morphic',
	#package : 'OSWindow-Core',
	#tag : 'Morphic'
}

{ #category : 'accessing' }
OSWorldRenderer class >> defaultExtent [

	^ 976@665
]

{ #category : 'accessing' }
OSWorldRenderer class >> isApplicableFor: aWorld [

	^ Smalltalk isHeadless and: [ CommandLineArguments new hasOption: 'interactive' ]
]

{ #category : 'accessing' }
OSWorldRenderer class >> priority [

	^ 2
]

{ #category : 'accessing' }
OSWorldRenderer >> activateCursor: aCursor withMask: maskForm [

	osWindow ifNotNil: [
		osWindow
			setMouseCursor: aCursor
			mask: maskForm
			andScale: world worldState worldRenderer screenScaleFactor ]
]

{ #category : 'accessing' }
OSWorldRenderer >> actualScreenSize [
	^ (self windowExtent / self screenScaleFactor) ceiling max: 1@1
]

{ #category : 'events' }
OSWorldRenderer >> checkForNewScreenSize [
	| windowRenderer |

	"Under certain circunstances, it may happen that renderer is still being
	 created when the UI process call this method. In that case, we need to
	 skip the check. "
	windowRenderer := self osWindowRenderer.
	windowRenderer ifNil: [ ^ self ].

	(display isNil or: [ display extent = self actualScreenSize ])
		ifTrue: [ ^ self ].

	display := Form extent: self actualScreenSize depth: 32.
	windowRenderer form: display.

	world worldState realWindowExtent: self actualScreenSize.

	world restoreMorphicDisplay
]

{ #category : 'accessing' }
OSWorldRenderer >> clipboardText [
	^ osWindow clipboardText
]

{ #category : 'accessing' }
OSWorldRenderer >> clipboardText: aString [
	osWindow clipboardText: aString asString
]

{ #category : 'utilities' }
OSWorldRenderer >> convertWindowMouseEventPosition: aPosition [

	^ (aPosition / self screenScaleFactor) rounded
]

{ #category : 'screenshot' }
OSWorldRenderer >> copyRectangle: aRectangle into: aForm [

	aForm
		copyBits: (aRectangle origin extent: aForm extent)
		from: display
		at: 0 @ 0
		clippingBox: aForm boundingBox
		rule: Form over
		fillColor: nil.

	^ aForm
]

{ #category : 'private' }
OSWorldRenderer >> deactivate [

	osWindow ifNotNil: [ osWindow destroy. osWindow := nil ].
	display := nil
]

{ #category : 'accessing' }
OSWorldRenderer >> defaultWindowIcon [

	^ self iconNamed: #pharoBig
]

{ #category : 'accessing' }
OSWorldRenderer >> defaultWindowTitle [

	^ Smalltalk image imageFile fullName
]

{ #category : 'operations' }
OSWorldRenderer >> deferUpdatesDuring: aBlock [

	self osWindowRenderer ifNotNil: [:renderer | renderer deferUpdatesWhile: aBlock ]
]

{ #category : 'private' }
OSWorldRenderer >> doActivate [

	| attributes initialExtent |
	initialExtent := world worldState realWindowExtent ifNil: [
		                 self class defaultExtent ].

	attributes := OSWindowAttributes new.
	attributes
		extent: initialExtent;
		title: self defaultWindowTitle;
		windowCentered: true;
		icon: self defaultWindowIcon.

	display := Form extent: initialExtent depth: 32.
	world extent: initialExtent.

	driver := self pickMostSuitableWindowDriver.
	attributes preferableDriver: driver.
	osWindow := OSWindow
		            createWithAttributes: attributes
		            eventHandler: (OSWindowMorphicEventHandler for: world).

	driver afterMainPharoWindowCreated: osWindow.
	driver
		afterSetWindowTitle: self defaultWindowTitle
		onWindow: osWindow.
	driver startUp: true.

	world worldState doFullRepaint.
	world displayWorld.

	OSWindowClipboard new beDefault.

	"SDL2 on MacOS presented a bug if this message send is done earlier on this method.
	See: https://github.com/pharo-project/pharo/issues/10981"
	osWindow focus
]

{ #category : 'operations' }
OSWorldRenderer >> drawDuring: aBlock [
	osWindow ifNil: [ ^ self ].
	self isProfilingRenderingTime ifTrue: [ ^ self drawWhileProfilingRenderingTimeDuring: aBlock ].
	self osWindowRenderer deferUpdatesWhile: [
		self osWindowRenderer drawDuring: aBlock
	]
]

{ #category : 'profiling' }
OSWorldRenderer >> drawWhileProfilingRenderingTimeDuring: aBlock [
	| measurements |
	previousFrameRenderingTime := [
		self osWindowRenderer deferUpdatesWhile: [
			self osWindowRenderer drawDuring: [ :canvas |
				| frameCanvasRenderingTime displayRectangle |
				frameCanvasRenderingTime := [ aBlock value: canvas ] timeToRunWithoutGC.

				measurements := {
					'Content' -> frameCanvasRenderingTime
				}.
				previousFrameRenderingTime ifNotNil: [
					measurements := measurements , {
						'Previous frame' -> previousFrameRenderingTime.
					}
				].

				displayRectangle := (0@0 extent: 250@40).
				canvas
					fillRectangle: displayRectangle color: Color white.
				measurements doWithIndex: [ :each :index |
					| line |
					line := self formatRenderingTimeMeasurement: each.
					canvas drawString: line at: 10@(10*index) font: nil color: Color black.
				].

				self osWindowRenderer updateRectangle: displayRectangle.
			]
		]
	] timeToRunWithoutGC.

	self logRenderingTimeMeasurements: measurements
]

{ #category : 'accessing' }
OSWorldRenderer >> driver [

	^ driver
]

{ #category : 'operations' }
OSWorldRenderer >> fullscreenMode: aValue [

	osWindow fullscreen: aValue.
	self checkForNewScreenSize
]

{ #category : 'events' }
OSWorldRenderer >> handleOSWindowCloseEvent: event [
	windowCloseAction value
]

{ #category : 'accessing' }
OSWorldRenderer >> icon: aForm [

	osWindow icon: aForm
]

{ #category : 'initialization' }
OSWorldRenderer >> initialize [

	super initialize.
	windowCloseAction := [ self currentWorld defer: [ WorldState quitSession ] ]
]

{ #category : 'accessing' }
OSWorldRenderer >> osWindow [
	^ osWindow
]

{ #category : 'accessing' }
OSWorldRenderer >> osWindowRenderer [

	osWindow ifNil: [ ^ nil ].
	^ osWindow renderer ifNil: [
		  osWindow newFormRenderer: display.
		  osWindow renderer ]
]

{ #category : 'private' }
OSWorldRenderer >> pickMostSuitableWindowDriver [

	driver := OSWindowDriver current.
	^ driver
]

{ #category : 'events' }
OSWorldRenderer >> requestStopTextEditing [

	osWindow stopTextInput
]

{ #category : 'events' }
OSWorldRenderer >> requestTextEditingAt: aRectangle [

	osWindow startTextInputAtRectangle: aRectangle
]

{ #category : 'accessing' }
OSWorldRenderer >> screenScaleFactor [

	^ world scaleFactor * self windowScaleFactor
]

{ #category : 'system startup' }
OSWorldRenderer >> shutDown: quitting [

	super shutDown: quitting.
	driver ifNotNil: [
		driver shutDown: quitting ]
]

{ #category : 'system startup' }
OSWorldRenderer >> startUp: resuming [

	super startUp: resuming.
	driver ifNotNil: [
		driver startUp: resuming ]
]

{ #category : 'operations' }
OSWorldRenderer >> updateDamage: allDamage [

	"quickly copy altered rects of canvas to Display:"
	self osWindowRenderer updateAreas: allDamage immediate: false
]

{ #category : 'events' }
OSWorldRenderer >> updateToNewResolution [

	self window updateToNewResolution
]

{ #category : 'activation' }
OSWorldRenderer >> updateWindowTitle [

	osWindow title: Smalltalk imageFile fullName.
	driver
		afterSetWindowTitle: Smalltalk imageFile fullName
		onWindow: osWindow
]

{ #category : 'accessing' }
OSWorldRenderer >> window [

	^ osWindow
]

{ #category : 'accessing' }
OSWorldRenderer >> windowCloseAction [
	^ windowCloseAction
]

{ #category : 'accessing' }
OSWorldRenderer >> windowCloseAction: anObject [
	windowCloseAction := anObject
]

{ #category : 'accessing' }
OSWorldRenderer >> windowExtent [

	^ osWindow ifNil: [ 240@120 ] ifNotNil: [ osWindow extent ]
]

{ #category : 'scale factor' }
OSWorldRenderer >> windowScaleFactor [
	^ osWindow ifNil: [ 1 ] ifNotNil: [ osWindow screenScaleFactor max: 1 ]
]

{ #category : 'accessing' }
OSWorldRenderer >> windowTitle: aString [

	osWindow title: aString
]

{ #category : 'accessing' }
OSWorldRenderer >> world: aWorld [

	world := aWorld
]
