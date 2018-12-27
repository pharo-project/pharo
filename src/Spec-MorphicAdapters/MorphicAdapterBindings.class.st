"
I am used to link the spec-adapter names to the morphic adapters
"
Class {
	#name : #MorphicAdapterBindings,
	#superclass : #SpecAdapterBindings,
	#category : #'Spec-MorphicAdapters-Support'
}

{ #category : #initialize }
MorphicAdapterBindings >> abstractAdapterClass [
	^ AbstractMorphicAdapter
]

{ #category : #fonts }
MorphicAdapterBindings >> defaultFont [

	^ StandardFonts defaultFont
]
