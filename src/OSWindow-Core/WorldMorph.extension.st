Extension { #name : #WorldMorph }

{ #category : #'*OSWindow-Core' }
WorldMorph >> checkNewWindowSize [
	| newExtent |
	newExtent := self osWindow extent.
	newExtent ~= self extent ifTrue: [
		self osWindowRenderer newExtent: newExtent.
		self extent: newExtent.
	]
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> checkSession [
	session == Smalltalk session ifFalse: [ 
		self recreateOSWindow.
		session := Smalltalk session.
	]
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> clipboardText [
	^ self osWindow clipboardText
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> clipboardText: aString [
	^ self osWindow clipboardText: aString
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> osWindow [
	
	self checkSession.
	^ osWindow
	

]

{ #category : #'*OSWindow-Core' }
WorldMorph >> osWindowRenderer [
	^ self osWindow renderer ifNil: [ 
		self osWindow newFormRenderer: Display.
		self osWindow renderer
	].
	

]

{ #category : #'*OSWindow-Core' }
WorldMorph >> pickMostSuitableWindowDriver [
	"TODO: check for headless mode"
	| driver |
	
	driver := OSWindowDriver current.
	
	"well, lets try using the VM driver"
	(driver isNullDriver and: [VMWindowDriver isSupported]) ifTrue: [ 
		^ VMWindowDriver new ].
	
	^ driver
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> recreateOSWindow [
	| attributes driver |
	session := Smalltalk session.
	attributes := OSWindowAttributes new.
	attributes
		extent: self extent;
		title: Smalltalk shortImageName;
		icon: (self iconNamed: #pharoIcon).
	driver := self pickMostSuitableWindowDriver.
	attributes preferableDriver: driver.
	osWindow := OSWindow createWithAttributes: attributes.
	osWindow newFormRenderer: Display.
	osWindow eventHandler: (OSWindowMorphicEventHandler for: self)
]

{ #category : #'*OSWindow-Core' }
WorldMorph >> updateOnOSWindow [
	"Update this world's display."

	| handsToDraw allDamage worldDamageRects handDamageRects canvas |

	submorphs do: [:m | m fullBounds].  "force re-layout if needed"
	worldState checkIfUpdateNeeded ifFalse: [^ self].  "display is already up-to-date"

	canvas := self osWindowRenderer getCanvas.
	
	self osWindowRenderer deferUpdatesWhile: [  
	
		worldDamageRects := worldState drawWorld: self submorphs: submorphs invalidAreasOn: canvas.  "repair world's damage on canvas"
		"self handsDo:[:h| h noticeDamageRects: worldDamageRects]."
		handsToDraw := worldState selectHandsToDrawForDamage: worldDamageRects.
		handDamageRects := handsToDraw collect: [:h | h savePatchFrom: canvas].
		allDamage := worldDamageRects, handDamageRects.

		handsToDraw reverseDo: [:h | canvas fullDrawMorph: h].  "draw hands onto world canvas"
 
	"*make this true to flash damaged areas for testing*"
		WorldState debugShowDamage ifTrue: [self flashRects: allDamage color: Color black].

		canvas finish.
	"quickly copy altered rects of canvas to Display:"
	
 
		self osWindowRenderer updateAreas: allDamage immediate: false.

 		handsToDraw do: [:h | h restoreSavedPatchOn: canvas].  "restore world canvas under hands"
	].
"	self osWindowRenderer forceDisplayUpdate."
	
]
