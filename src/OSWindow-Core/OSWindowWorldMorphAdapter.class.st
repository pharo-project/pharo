"
I am a Spec adapter that uses a WorldMorph in an external OSWindow.

"
Class {
	#name : #OSWindowWorldMorphAdapter,
	#superclass : #MorphicWindowAdapter,
	#category : #'OSWindow-Core-Morphic'
}

{ #category : #factory }
OSWindowWorldMorphAdapter >> buildWidget [
	^ OSWindowWorldMorphWithModel new
		layoutPolicy: ProportionalLayout new;
		model: model;
		isResizeable: self isResizeable;
		yourself
]
