"
I'm the adapter used to bridge a ToolBarButton and its morphic representation.
"
Class {
	#name : #SpMorphicToolBarButtonAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToolBarButtonAdapter >> buildWidget [
	| button |
	
	button := (SpToolBarButtonMorph
			on: self model
			getState: nil 
			action:  #execute)
		helpText: self model help;
		beIconTop;
		hResizing: #rigid;
		vResizing: #spaceFill;
		borderWidth: 0;
		borderColor: Color transparent;		
		cellPositioning: #center;
		yourself.
		
	^ button
]
