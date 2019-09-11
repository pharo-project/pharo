Class {
	#name : #SpMorphicAthensStaticAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicAthensStaticAdapter >> buildWidget [
	| instance |
	instance := SpAthensMorph new.
	self presenter
		whenDrawBlockChangedDo: [ :newBlock | instance drawBlock: newBlock ].
	self presenter
		whenExtentChangedDo: [ : newExtent | instance extent: newExtent ].
	instance extent: self presenter surfaceExtent.
	instance drawBlock: self presenter drawBlock.
	instance setBalloonText: self model help.
	self presenter
		whenHelpChangedDo: [ instance setBalloonText: self model help ].
	^ instance
]

{ #category : #drawing }
SpMorphicAthensStaticAdapter >> redraw [
	widget redraw
]

{ #category : #accessing }
SpMorphicAthensStaticAdapter >> surface [
	^ widget surface
]
