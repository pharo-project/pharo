Class {
	#name : #SpMorphicButtonBarAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #accessing }
SpMorphicButtonBarAdapter class >> defaultHeight [

	^ 30
]

{ #category : #accessing }
SpMorphicButtonBarAdapter class >> defaultItemSeparation [
	
	^ 3@0
]

{ #category : #factory }
SpMorphicButtonBarAdapter >> addModelTo: panelMorph [

	self model items do: [ :each |
		self model focusOrder add: each.
		panelMorph addMorph: (each buildWithSpec
			width: 100;
			hResizing: #rigid;
			yourself) ]
]

{ #category : #factory }
SpMorphicButtonBarAdapter >> buildWidget [
	| panelMorph |
	
	panelMorph := PanelMorph new 
		changeTableLayout;
		listDirection: self listDirection;
		hResizing: #spaceFill;
		vResizing: #rigid;
		cellInset: self class defaultItemSeparation;
		height: self class defaultHeight;
		yourself.

	self addModelTo: panelMorph.
	
	^ panelMorph
]

{ #category : #factory }
SpMorphicButtonBarAdapter >> listDirection [
	
	self model isPlaceAtEnd ifTrue: [ ^ #rightToLeft ].
	self model isPlaceAtStart ifTrue: [ ^ #leftToRight ].
	
	self error: 'Undefined location to place buttons.'
]
