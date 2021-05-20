"
I'm the adapter used to bridge a ToolBarButton and its morphic representation.
"
Class {
	#name : #SpMorphicToolbarButtonAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #accessing }
SpMorphicToolbarButtonAdapter >> badge [

	^ self presenter badge
]

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

{ #category : #accessing }
SpMorphicToolbarButtonAdapter >> enabled: aBoolean [

	super enabled: aBoolean.
	self updateIcon
]

{ #category : #execution }
SpMorphicToolbarButtonAdapter >> execute [

	self presenter action cull: self presenter
]

{ #category : #accessing }
SpMorphicToolbarButtonAdapter >> icon [
	| icon |
	
	icon := self presenter icon.
	icon ifNil: [ ^ nil ].
	
	^ self presenter isEnabled
		ifTrue: [ icon ]
		ifFalse: [ icon asGrayScaleWithAlpha ]
]

{ #category : #accessing }
SpMorphicToolbarButtonAdapter >> label [

	^ self presenter label
]

{ #category : #factory }
SpMorphicToolbarButtonAdapter >> morphClass [

	^ SpToolbarButtonMorph
]

{ #category : #factory }
SpMorphicToolbarButtonAdapter >> newButton [
	
	^ (self morphClass
			on: self
			getState: nil 
			action: #execute)
		helpText: self presenter help;
		beIconTop;
		hResizing: #shrinkWrap;
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
