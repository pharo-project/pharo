"
I'm an adapter to bridge to Athens using an instance of class `SpAthensMorph` as widget
"
Class {
	#name : #SpMorphicAthensAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicAthensAdapter >> buildWidget [

	| instance |
	instance := SpAthensMorph new.
	self presenter whenDrawBlockChangedDo: [ :newBlock | 
		instance drawBlock: newBlock ].
	self presenter whenExtentChangedDo: [ :newExtent | 
		instance extent: newExtent ].
	instance extent: self presenter surfaceExtent.
	instance drawBlock: self presenter drawBlock.
	instance setBalloonText: self model help.
	self presenter whenHelpChangedDo: [ 
		instance setBalloonText: self model help ].
	^ instance
]

{ #category : #drawing }
SpMorphicAthensAdapter >> redraw [

	widget redraw
]

{ #category : #accessing }
SpMorphicAthensAdapter >> surface [

	^ widget surface
]
