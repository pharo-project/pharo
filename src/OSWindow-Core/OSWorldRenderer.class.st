"
I know how to render the world in an OSWindow
"
Class {
	#name : #OSWorldRenderer,
	#superclass : #AbstractWorldRenderer,
	#instVars : [
		'driver',
		'osWindow',
		'display',
		'windowCloseAction',
		'previousFrameRenderingTime'
	],
	#category : #'OSWindow-Core-Morphic'
}

{ #category : #accessing }
OSWorldRenderer class >> isApplicableFor: aWorld [

	^ Smalltalk isHeadless and: [ CommandLineArguments new hasOption: 'interactive' ]
]

{ #category : #accessing }
OSWorldRenderer class >> priority [ 

	^ 2
]

{ #category : #accessing }
OSWorldRenderer >> activateCursor: aCursor withMask: maskForm [

	osWindow ifNotNil: [ 
		osWindow
			setMouseCursor: aCursor
			mask: maskForm
			andScale: world worldState worldRenderer screenScaleFactor ]
]

{ #category : #accessing }
OSWorldRenderer >> actualScreenSize [
	^ (self windowExtent / self screenScaleFactor) ceiling max: 1@1
]

{ #category : #private }
OSWorldRenderer >> checkForNewScreenSize [

	(display isNil or: [ display extent = self actualScreenSize ]) 
		ifTrue: [ ^ self ].

	display := Form extent: self actualScreenSize depth: 32.
	self osWindowRenderer form: display.

	world worldState realWindowExtent: self actualScreenSize.

	world restoreMorphicDisplay
]

{ #category : #accessing }
OSWorldRenderer >> clipboardText [
	^ osWindow clipboardText
]

{ #category : #accessing }
OSWorldRenderer >> clipboardText: aString [
	osWindow clipboardText: aString asString
]

{ #category : #utilities }
OSWorldRenderer >> convertWindowMouseEventPosition: aPosition [

	^ (aPosition / self screenScaleFactor) rounded
]

{ #category : #private }
OSWorldRenderer >> deactivate [

	osWindow ifNotNil: [ osWindow destroy. osWindow := nil ].
	display := nil.
	
]

{ #category : #operations }
OSWorldRenderer >> deferUpdatesDuring: aBlock [

	self osWindowRenderer ifNotNil: [:renderer | renderer deferUpdatesWhile: aBlock ]
]

{ #category : #private }
OSWorldRenderer >> doActivate [

	| attributes initialExtent |
	
	initialExtent := world worldState realWindowExtent ifNil: [976@665].

	attributes := OSWindowAttributes new.
	attributes
		extent: initialExtent;
		title: Smalltalk image imageFile fullName;
		windowCentered:true;
		icon: (self iconNamed: #pharoBig).

	display := Form extent: initialExtent depth: 32.
	world extent: initialExtent.

	driver := self pickMostSuitableWindowDriver.
	attributes preferableDriver: driver.
	osWindow := OSWindow createWithAttributes: attributes eventHandler: (OSWindowMorphicEventHandler for: world).
	
	driver afterSetWindowTitle: Smalltalk image imageFile fullName onWindow: osWindow.
		
	osWindow focus. 
	
	world worldState doFullRepaint.
	world displayWorld.
	
	OSWindowClipboard new beDefault.
]

{ #category : #operations }
OSWorldRenderer >> drawDuring: aBlock [
	osWindow ifNil: [ ^ self ].
	self isProfilingRenderingTime ifTrue: [ ^ self drawWhileProfilingRenderingTimeDuring: aBlock ].
	self osWindowRenderer deferUpdatesWhile: [
		self osWindowRenderer drawDuring: aBlock
	].
]

{ #category : #profiling }
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

{ #category : #accessing }
OSWorldRenderer >> driver [

	^ driver
]

{ #category : #operations }
OSWorldRenderer >> fullscreenMode: aValue [

	osWindow fullscreen: aValue.
	self checkForNewScreenSize
]

{ #category : #events }
OSWorldRenderer >> handleOSWindowCloseEvent: event [
	windowCloseAction value
]

{ #category : #accessing }
OSWorldRenderer >> icon: aForm [ 

	osWindow icon: aForm
]

{ #category : #initialization }
OSWorldRenderer >> initialize [

	super initialize.
	windowCloseAction := [ self currentWorld defer: [ WorldState quitSession ] ]
]

{ #category : #accessing }
OSWorldRenderer >> osWindowRenderer [

	osWindow ifNil: [ ^ nil ].
	^ osWindow renderer ifNil: [ 
		  osWindow newFormRenderer: display.
		  osWindow renderer ]
]

{ #category : #private }
OSWorldRenderer >> pickMostSuitableWindowDriver [

	driver := OSWindowDriver current.
	^ driver
]

{ #category : #accessing }
OSWorldRenderer >> screenScaleFactor [

	^ world scaleFactor * self windowScaleFactor
]

{ #category : #operations }
OSWorldRenderer >> updateDamage: allDamage [

	"quickly copy altered rects of canvas to Display:"
	self osWindowRenderer updateAreas: allDamage immediate: false.
]

{ #category : #activation }
OSWorldRenderer >> updateWindowTitle [

	osWindow title: Smalltalk imageFile fullName.
	driver
		afterSetWindowTitle: Smalltalk imageFile fullName
		onWindow: osWindow
]

{ #category : #accessing }
OSWorldRenderer >> window [
	
	^ osWindow 
]

{ #category : #accessing }
OSWorldRenderer >> windowCloseAction [
	^ windowCloseAction
]

{ #category : #accessing }
OSWorldRenderer >> windowCloseAction: anObject [
	windowCloseAction := anObject
]

{ #category : #accessing }
OSWorldRenderer >> windowExtent [

	^ osWindow ifNil: [ 240@120 ] ifNotNil: [ osWindow extent ]
]

{ #category : #'scale factor' }
OSWorldRenderer >> windowScaleFactor [
	^ osWindow ifNil: [ 1 ] ifNotNil: [ osWindow screenScaleFactor max: 1 ]
]

{ #category : #accessing }
OSWorldRenderer >> windowTitle: aString [

	osWindow title: aString
]

{ #category : #accessing }
OSWorldRenderer >> world: aWorld [

	world := aWorld
]
