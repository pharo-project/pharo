Class {
	#name : #MorphicAthensAnimatedAdapter,
	#superclass : #MorphicAthensStaticAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #initialization }
MorphicAthensAnimatedAdapter >> updateModelToMoment: tick [
	self widget changed
]
