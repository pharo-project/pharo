"
I am a morphic specific adapter used to build a transfer object during a drang and drop operation
"
Class {
	#name : #MorphicTransferAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #Spec-MorphicAdapters
}

{ #category : #factory }
MorphicTransferAdapter >> buildWidget [

	^ SpecTransferMorph new
		model: self;
		passenger: self passenger;
		source: self source;
		yourself
]

{ #category : #'widget API' }
MorphicTransferAdapter >> passenger [

	^ [ self model passenger ]
]

{ #category : #'widget API' }
MorphicTransferAdapter >> source [

	^ [ self model source ]
]
