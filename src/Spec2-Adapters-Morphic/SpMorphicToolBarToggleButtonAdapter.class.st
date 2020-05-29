"
I'm the adapter used to bridge a ToolBarToggleButton and its morphic representation.
"
Class {
	#name : #SpMorphicToolBarToggleButtonAdapter,
	#superclass : #SpMorphicToolBarButtonAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToolBarToggleButtonAdapter >> morphClass [ 

	^ SpToolBarToggleButtonMorph
]

{ #category : #factory }
SpMorphicToolBarToggleButtonAdapter >> newButton [

	^ super newButton
		action: #execute:
		yourself
]
