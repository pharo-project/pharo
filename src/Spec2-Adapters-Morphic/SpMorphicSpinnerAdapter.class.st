"
I'm the adapter used to bridge a Spinner and its morphic representation.
"
Class {
	#name : #SpMorphicSpinnerAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'subMenuWidget'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicSpinnerAdapter >> buildWidget [
	^ SpSpinnerMorph new
]
