Class {
	#name : #SpMorphicAthensAnimatedAdapter,
	#superclass : #SpMorphicAthensStaticAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #initialization }
SpMorphicAthensAnimatedAdapter >> updateModelToMoment: tick [
	self widget changed
]
