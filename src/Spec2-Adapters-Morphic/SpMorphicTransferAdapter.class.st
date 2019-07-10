"
I am a morphic specific adapter used to build a transfer object during a drang and drop operation
"
Class {
	#name : #SpMorphicTransferAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicTransferAdapter >> buildWidget [

	^ SpTransferMorph new
		model: self;
		passenger: self passenger;
		source: self source;
		yourself
]

{ #category : #'widget API' }
SpMorphicTransferAdapter >> passenger [

	^ [ self model passenger ]
]

{ #category : #'widget API' }
SpMorphicTransferAdapter >> source [

	^ [ self model source ]
]
