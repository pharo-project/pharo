"
I'm the adapter used to bridge a ToolBarButton and its morphic representation.
"
Class {
	#name : #SpMorphicToolbarButtonAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToolbarButtonAdapter >> buildWidget [
	| button |
	
	button := self newButton.
	
	self presenter whenLabelChangedDo: [ self updateLabel ].
	self presenter whenIconChangedDo: [ self updateIcon ].

	^ button
]

{ #category : #updating }
SpMorphicToolbarButtonAdapter >> button [

	^ widget
]

{ #category : #factory }
SpMorphicToolbarButtonAdapter >> morphClass [

	^ SpToolbarButtonMorph
]

{ #category : #factory }
SpMorphicToolbarButtonAdapter >> newButton [
	
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
SpMorphicToolbarButtonAdapter >> updateIcon [

	self button update: self button getIconSelector
]

{ #category : #updating }
SpMorphicToolbarButtonAdapter >> updateLabel [

	self button update: self button getLabelSelector
]
