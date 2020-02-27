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
		'windowCloseAction'
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

	osWindow ifNotNil: [ osWindow setMouseCursor: aCursor mask: maskForm ]
]

{ #category : #initialization }
OSWorldRenderer >> actualScreenSize [

	^ osWindow ifNil: [ 240@120 ] ifNotNil: [ osWindow extent ]
]

{ #category : #initialization }
OSWorldRenderer >> canvas [ 

	^ self osWindowRenderer getCanvas
]

{ #category : #initialization }
OSWorldRenderer >> canvas: x [

	self flag: #jajaja
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
OSWorldRenderer >> deactivate [

	osWindow ifNotNil: [ osWindow destroy. osWindow := nil ].
	display := nil.
	
]

{ #category : #initialization }
OSWorldRenderer >> deferUpdates: aBoolean [

	^ false
]

{ #category : #operations }
OSWorldRenderer >> displayWorldState: aWorldState ofWorld: aWorld submorphs: submorphs [
	"Update this world's display."

	| handsToDraw allDamage worldDamageRects handDamageRects |

	submorphs do: [:m | m fullBounds].  "force re-layout if needed"

	aWorldState checkIfUpdateNeeded ifFalse: [^ self].  
	"display is already up-to-date"
	
	osWindow ifNil: [ ^ self ].

	self osWindowRenderer deferUpdatesWhile: [  
		
		worldDamageRects := aWorldState drawWorld: aWorld submorphs: submorphs invalidAreasOn: self canvas.  
		"repair world's damage on canvas"

		handsToDraw := aWorldState selectHandsToDrawForDamage: worldDamageRects.
		handDamageRects := handsToDraw collect: [:h | h savePatchFrom: self canvas].
		allDamage := worldDamageRects, handDamageRects.

		handsToDraw reverseDo: [:h | self canvas fullDrawMorph: h].  "draw hands onto world canvas"
 
	"*make this true to flash damaged areas for testing*"
		WorldState debugShowDamage ifTrue: [aWorld flashRects: allDamage color: Color black].

		self canvas finish.
	"quickly copy altered rects of canvas to Display:"	
 
		self osWindowRenderer updateAreas: allDamage immediate: false.

 		handsToDraw do: [:h | h restoreSavedPatchOn: self canvas].  "restore world canvas under hands"
	].
"	self osWindowRenderer forceDisplayUpdate."
	
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
	
	osWindow focus. 
	
	world worldState doFullRepaint.
	world displayWorld.
	
	OSWindowClipboard new beDefault.
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

{ #category : #accessing }
OSWorldRenderer >> windowCloseAction [
	^ windowCloseAction
]

{ #category : #accessing }
OSWorldRenderer >> windowCloseAction: anObject [
	windowCloseAction := anObject
]

{ #category : #initialization }
OSWorldRenderer >> world: aWorld [

	world := aWorld
]
