"
Description
--------------------

I am a Morphic adapter to bridge the ProgressBarPresenter and the ProgressBarMorph.
"
Class {
	#name : #SpMorphicProgressBarAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicProgressBarAdapter >> buildWidget [
	| m p |
	m := Morph new
		changeProportionalLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.

	p := (self progressBarState progressBarMorph from: 0 to: 1)
		changeProportionalLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.

	self progressBarState customizeMorphicBar: p.

	m addMorph: p fullFrame: LayoutFrame identity.
	^ m
]

{ #category : #accessing }
SpMorphicProgressBarAdapter >> progressBarState [
	^ self model state
]
