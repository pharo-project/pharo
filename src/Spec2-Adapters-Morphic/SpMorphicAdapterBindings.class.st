"
I am used to link the spec-adapter names to the morphic adapters
"
Class {
	#name : #SpMorphicAdapterBindings,
	#superclass : #SpAdapterBindings,
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #initialize }
SpMorphicAdapterBindings >> abstractAdapterClass [
	^ SpAbstractMorphicAdapter
]
