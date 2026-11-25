"
An AthensWrapWorldMorph is a AthensWrapMorph holding a specialized Athens WorldMorph. This Athens WorldMorph draws the submorphs with an AthensCanvas.

Instance Variables
	worldmorph:		the Athens WorldMorph
	wrappedWorldDisplay:		a display wrapper

"
Class {
	#name : 'AthensWrapWorldMorph',
	#superclass : 'AthensWrapMorph',
	#instVars : [
		'worldmorph',
		'wrappedWorldDisplay'
	],
	#category : 'Athens-Morphic',
	#package : 'Athens-Morphic'
}

{ #category : 'instance creation' }
AthensWrapWorldMorph class >> on: aWorld [

	^ self new
		  embeddedWorld: aWorld;
		  yourself
]

{ #category : 'initialization' }
AthensWrapWorldMorph >> defaultExtent [
	^ 400 @ 400
]

{ #category : 'accessing' }
AthensWrapWorldMorph >> embeddedWorld [

	^ worldmorph
]

{ #category : 'initialization' }
AthensWrapWorldMorph >> embeddedWorld: aWorld [
	"Embed aWorld, a plain world or a desktop, drawn with Athens"

	worldmorph ifNotNil: [ worldmorph delete ].
	worldmorph := aWorld.
	aWorld worldState worldRenderer: ((AthensWrappedWorldRenderer forWorld: aWorld)
		 display: wrappedWorldDisplay;
		 yourself).
	aWorld hands ifEmpty: [ aWorld addHand: HandMorph new ].
	self addMorph: aWorld.
	aWorld extent: self extent
]

{ #category : 'initialization' }
AthensWrapWorldMorph >> initialize [

	super initialize.
	self extent: self defaultExtent.
	self hResizing: #spaceFill.
	self vResizing: #spaceFill.
	wrappedWorldDisplay := AthensWrappedWorldDisplay on: self.
	self embeddedWorld: (WorldMorph new
		 color: Color orange;
		 yourself)
]

{ #category : 'layout' }
AthensWrapWorldMorph >> layoutChanged [
	super layoutChanged.
	worldmorph ifNotNil:[:w |  w extent: self extent]
]

{ #category : 'stepping' }
AthensWrapWorldMorph >> step [
	self changed
]

{ #category : 'stepping' }
AthensWrapWorldMorph >> stepTime [
	^ 50
]
