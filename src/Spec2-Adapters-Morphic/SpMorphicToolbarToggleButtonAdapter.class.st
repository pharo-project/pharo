"
I'm the adapter used to bridge a ToolBarToggleButton and its morphic representation.
"
Class {
	#name : #SpMorphicToolbarToggleButtonAdapter,
	#superclass : #SpMorphicToolbarButtonAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToolbarToggleButtonAdapter >> morphClass [ 

	^ SpToolbarToggleButtonMorph
]

{ #category : #factory }
SpMorphicToolbarToggleButtonAdapter >> newButton [

	^ super newButton
		action: #execute:
		yourself
]
