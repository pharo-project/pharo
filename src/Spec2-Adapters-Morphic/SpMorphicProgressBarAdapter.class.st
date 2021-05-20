"
Description
--------------------

I am a Morphic adapter to bridge the ProgressBarPresenter and the ProgressBarMorph.
"
Class {
	#name : #SpMorphicProgressBarAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#instVars : [
		'progressBar'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicProgressBarAdapter >> buildWidget [
	| panel |
	
	panel := Morph new
		changeProportionalLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.

	progressBar := (self progressBarState progressBarMorph from: 0 to: 1)
		     changeProportionalLayout;
		     hResizing: #spaceFill;
		     vResizing: #spaceFill;
		     yourself.

	self progressBarState customizeMorphicBar: progressBar.

	panel 
		addMorph: progressBar
		fullFrame: LayoutFrame identity.
	
	^ panel
]

{ #category : #accessing }
SpMorphicProgressBarAdapter >> progressBarState [

	^ self model state
]

{ #category : #updating }
SpMorphicProgressBarAdapter >> updateState [

	self progressBarState customizeMorphicBar: progressBar

]
