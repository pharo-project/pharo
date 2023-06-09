"
An AthensWorldMorph is a special version of a WorldMorph.
It is within a real Morph in a real morphic world. The Morph it is in defines the DisplayScreen dimension. (see AthensWrappedWorldDisplay)
"
Class {
	#name : #AthensWorldMorph,
	#superclass : #WorldMorph,
	#category : #'Athens-Morphic'
}

{ #category : #'world state' }
AthensWorldMorph >> display [
	worldState display
]

{ #category : #'world state' }
AthensWorldMorph >> display: aWrappedDisplay [
	worldState display: aWrappedDisplay
]

{ #category : #initialization }
AthensWorldMorph >> initialize [
	super initialize.
	worldState := AthensWorldState new
]

{ #category : #'world state' }
AthensWorldMorph >> restoreMorphicDisplay [
"overwritten for using its own display dimension"
	DisplayScreen startUp.

	self
		extent: self display extent;
		viewBox: self display boundingBox;
		handsDo: [:h | h visible: true; showTemporaryCursor: nil];
		resizeBackgroundMorph;
		submorphsDo: [ :each | each displayExtentChanged ];
		fullRepaintNeeded.

	self defer: [Cursor normal show].

	self layoutChanged
]
