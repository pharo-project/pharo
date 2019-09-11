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
