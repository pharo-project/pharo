Class {
	#name : #SpMorphicToolBarToggleButtonAdapter,
	#superclass : #SpMorphicToolBarButtonAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToolBarToggleButtonAdapter >> buildWidget [

	^ super buildWidget
		action: #execute:
		yourself
]

{ #category : #factory }
SpMorphicToolBarToggleButtonAdapter >> morphClass [ 

	^ SpToolBarToggleButtonMorph
]
