Class {
	#name : #MorphicSpinnerAdapter,
	#superclass : #AbstractMorphicAdapter,
	#instVars : [
		'subMenuWidget'
	],
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicSpinnerAdapter >> buildWidget [
	^ SpinnerMorph new
]
