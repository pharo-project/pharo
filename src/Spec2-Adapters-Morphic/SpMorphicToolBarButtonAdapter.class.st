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
	
	button := self newButton.
	
	self presenter whenLabelChangedDo: [ self updateLabel ].
	self presenter whenIconChangedDo: [ self updateIcon ].

	^ button
]

{ #category : #updating }
SpMorphicToolBarButtonAdapter >> button [

	^ widget
]

{ #category : #factory }
SpMorphicToolBarButtonAdapter >> morphClass [

	^ SpToolBarButtonMorph
]

{ #category : #factory }
SpMorphicToolBarButtonAdapter >> newButton [
	
	^ (self morphClass
			on: self presenter
			getState: nil 
			action: #execute)
		helpText: self model help;
		beIconTop;
		hResizing: #rigid;
		vResizing: #spaceFill;
		borderWidth: 0;
		borderColor: Color transparent;		
		cellPositioning: #center;
		yourself
]

{ #category : #updating }
SpMorphicToolBarButtonAdapter >> updateIcon [

	self button update: self button getIconSelector
]

{ #category : #updating }
SpMorphicToolBarButtonAdapter >> updateLabel [

	self button update: self button getLabelSelector
]
