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

	^ Smalltalk isHeadless and: [ CommandLineArguments new hasOption: 'interactive'  ]
]

{ #category : #accessing }
OSWorldRenderer class >> priority [ 

	^ 2
]

{ #category : #initialization }
OSWorldRenderer >> activateCursor: aCursor withMask: maskForm [

	osWindow ifNotNil: [ osWindow setMouseCursor: aCursor mask: maskForm andScale: world worldState worldRenderer screenScaleFactor ]
]

{ #category : #initialization }
OSWorldRenderer >> actualScreenSize [
	^ (self windowExtent / self screenScaleFactor) ceiling max: 1@1
]

{ #category : #initialization }
OSWorldRenderer >> checkForNewScreenSize [

	(display isNil or: [display extent = self actualScreenSize]) 
		ifTrue: [ ^ self ].

	display := Form extent: self actualScreenSize depth: 32.
	self osWindowRenderer form: display.

	world worldState realWindowExtent: self actualScreenSize.

	world restoreMorphicDisplay.

]

{ #category : #initialization }
OSWorldRenderer >> clipboardText [
	^ osWindow clipboardText
]

{ #category : #initialization }
OSWorldRenderer >> clipboardText: aString [
	osWindow clipboardText: aString asString
]

{ #category : #initialization }
OSWorldRenderer >> convertWindowMouseEventPosition: aPosition [

	^ (aPosition / self screenScaleFactor) rounded
]

{ #category : #initialization }
OSWorldRenderer >> deactivate [

	osWindow ifNotNil: [ osWindow destroy. osWindow := nil ].
	display := nil.
	
]

{ #category : #operations }
OSWorldRenderer >> deferUpdatesDuring: aBlock [

	self osWindowRenderer ifNotNil: [:renderer | renderer deferUpdatesWhile: aBlock ]
]

{ #category : #initialization }
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

{ #category : #initialization }
OSWorldRenderer >> driver [

	^ driver
]

{ #category : #operations }
OSWorldRenderer >> fullscreenMode: aValue [

	osWindow fullscreen: aValue.
	self checkForNewScreenSize.
]

{ #category : #events }
OSWorldRenderer >> handleOSWindowCloseEvent: event [
	windowCloseAction value
]

{ #category : #accessing }
OSWorldRenderer >> icon: aForm [ 

	osWindow icon: aForm
]

{ #category : #activation }
OSWorldRenderer >> initialize [
	super initialize.
	windowCloseAction := [ self currentWorld defer: [WorldState quitSession] ].
]

{ #category : #initialization }
OSWorldRenderer >> osWindowRenderer [
	osWindow ifNil: [ ^ nil ].
	^ osWindow renderer ifNil: [
		osWindow newFormRenderer: display.
		osWindow renderer
	].
	
]

{ #category : #initialization }
OSWorldRenderer >> pickMostSuitableWindowDriver [	
	driver := OSWindowDriver current.
	
	^ driver
]

{ #category : #initialization }
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

{ #category : #initialization }
OSWorldRenderer >> windowExtent [

	^ osWindow ifNil: [ 240@120 ] ifNotNil: [ osWindow extent ]
]

{ #category : #'scale factor' }
OSWorldRenderer >> windowScaleFactor [
	^ osWindow ifNil: [ 1 ] ifNotNil: [ osWindow screenScaleFactor max: 1 ]
]

{ #category : #initialization }
OSWorldRenderer >> world: aWorld [

	world := aWorld
]
