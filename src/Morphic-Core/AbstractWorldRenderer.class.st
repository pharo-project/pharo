"
I implement the logic to render a world.
I have different implementations to start the world.
"
Class {
	#name : #AbstractWorldRenderer,
	#superclass : #Object,
	#instVars : [
		'world',
		'alreadyActivated'
	],
	#classVars : [
		'MainWorldRenderer'
	],
	#category : #'Morphic-Core-Worlds'
}

{ #category : #accessing }
AbstractWorldRenderer class >> detectCorrectOneforWorld: aWorld [

	| aRenderer |

	MainWorldRenderer ifNotNil: [ ^ MainWorldRenderer ].

	(self allSubclasses sorted: [ :a :b | a priority > b priority ])
		detect: [ :aClass | aClass isApplicableFor: aWorld ]
		ifFound: [ :aClass | aRenderer := aClass forWorld: aWorld ].
		
	^ MainWorldRenderer := aRenderer
]

{ #category : #accessing }
AbstractWorldRenderer class >> forWorld: aWorld [

	^ self new 
		world: aWorld;
		yourself
]

{ #category : #accessing }
AbstractWorldRenderer class >> initialize [

	SessionManager default registerSystemClassNamed: self name	
]

{ #category : #accessing }
AbstractWorldRenderer class >> priority [ 

	^ 0
]

{ #category : #accessing }
AbstractWorldRenderer class >> shutDown: quitting [
	quitting
		ifFalse: [ ^ self ].

	[ MainWorldRenderer ifNotNil: [ :e | e deactivate ] ]
		ensure: [ MainWorldRenderer := nil ]
]

{ #category : #accessing }
AbstractWorldRenderer class >> startUp: isANewSession [

	isANewSession
		ifTrue: [ MainWorldRenderer := nil ]
]

{ #category : #activation }
AbstractWorldRenderer >> activate [

	alreadyActivated ifTrue: [ ^ self ].

	self doActivate.
	alreadyActivated := true
]

{ #category : #activation }
AbstractWorldRenderer >> actualScreenSize [

	self subclassResponsibility 
]

{ #category : #activation }
AbstractWorldRenderer >> deactivate [

	self subclassResponsibility 
]

{ #category : #operations }
AbstractWorldRenderer >> deferUpdates: aBoolean [

	^ false
]

{ #category : #rendering }
AbstractWorldRenderer >> deferUpdatesDuring: aBlock [

	^ aBlock value
]

{ #category : #rendering }
AbstractWorldRenderer >> displayWorldState: aWorldState ofWorld: aWorld [
	"Update this world's display."

	| submorphs |
	submorphs := aWorld submorphs.

	"force re-layout if needed"
	submorphs do: [ :m | m fullBounds ].

	"display is already up-to-date"
	aWorldState checkIfUpdateNeeded
		ifFalse: [ ^ self ].

	self deferUpdatesDuring: [
		self drawDuring: [ :aMorphicCanvas | | worldDamageRects handDamageRects allDamage handsToDraw |
			worldDamageRects := aWorldState drawWorld: aWorld submorphs: submorphs invalidAreasOn: aMorphicCanvas.

			"repair world's damage on canvas"
			handsToDraw := aWorldState selectHandsToDrawForDamage: worldDamageRects.
			handDamageRects := handsToDraw collect: [:h | h savePatchFrom: aMorphicCanvas].
			allDamage := worldDamageRects, handDamageRects.

			"draw hands onto world canvas"
			handsToDraw reverseDo: [:h | aMorphicCanvas fullDrawMorph: h].

			"*make this true to flash damaged areas for testing*"
			WorldState debugShowDamage ifTrue: [aWorld flashRects: allDamage color: Color black].

			aMorphicCanvas finish.
			self updateDamage: allDamage.

			"restore world canvas under hands"
	 		handsToDraw do: [:h | h restoreSavedPatchOn: aMorphicCanvas ].
		] ]
]

{ #category : #activation }
AbstractWorldRenderer >> doActivate [

	self subclassResponsibility 
]

{ #category : #rendering }
AbstractWorldRenderer >> drawDuring: aBlock [

	self subclassResponsibility
]

{ #category : #accessing }
AbstractWorldRenderer >> icon: aForm [ 

	self subclassResponsibility
]

{ #category : #activation }
AbstractWorldRenderer >> initialize [

	alreadyActivated := false
]

{ #category : #rendering }
AbstractWorldRenderer >> invalidate [
	"Invalidate the entire render"
	"Each subclass should define me properly"
]

{ #category : #operations }
AbstractWorldRenderer >> restoreMorphicDisplay [ 

	world
		extent: self actualScreenSize;
		viewBox: self viewBox;
		handsDo: [:h | h visible: true; showTemporaryCursor: nil];
		resizeBackgroundMorph;
		submorphsDo: [ :each | each displayExtentChanged ];
		fullRepaintNeeded.
		
	world defer: [Cursor normal show].
		
	world layoutChanged.

]

{ #category : #rendering }
AbstractWorldRenderer >> updateDamage: rectangles [

	self subclassResponsibility
]

{ #category : #initialization }
AbstractWorldRenderer >> usableArea [

	^ self viewBox
]

{ #category : #'display box access' }
AbstractWorldRenderer >> viewBox [ 

	^ 0@0 corner: self actualScreenSize
]

{ #category : #accessing }
AbstractWorldRenderer >> world [
	^ world
]

{ #category : #accessing }
AbstractWorldRenderer >> world: anObject [
	world := anObject
]
