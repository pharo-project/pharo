"
I implement the logic to render a world.
I have different implementations to start the world.
"
Class {
	#name : #AbstractWorldRenderer,
	#superclass : #Object,
	#instVars : [
		'world'
	],
	#category : #'Morphic-Core-Worlds'
}

{ #category : #accessing }
AbstractWorldRenderer class >> detectCorrectOneforWorld: aWorld [

(self allSubclasses sorted: [ :a :b | a priority > b priority ])
        detect: [ :aClass | aClass isApplicableFor: aWorld ]
        ifFound: [ :aClass | ^ aClass forWorld: aWorld ]
]

{ #category : #accessing }
AbstractWorldRenderer class >> forWorld: aWorld [

	^ self new 
		world: aWorld;
		yourself
]

{ #category : #accessing }
AbstractWorldRenderer class >> priority [ 

	^ 0
]

{ #category : #activation }
AbstractWorldRenderer >> activate [

	self subclassResponsibility 
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
