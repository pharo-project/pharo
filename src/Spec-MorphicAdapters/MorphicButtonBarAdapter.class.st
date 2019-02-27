Class {
	#name : #MorphicButtonBarAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #accessing }
MorphicButtonBarAdapter class >> defaultHeight [

	^ 30
]

{ #category : #accessing }
MorphicButtonBarAdapter class >> defaultItemSeparation [
	
	^ 3@0
]

{ #category : #factory }
MorphicButtonBarAdapter >> addModelTo: panelMorph [
	self model items do: [ :each |
		self model focusOrder add: each.
		panelMorph addMorph: (each buildWithSpec
			width: 100;
			hResizing: #rigid;
			yourself) ]
]

{ #category : #factory }
MorphicButtonBarAdapter >> buildWidget [
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
MorphicButtonBarAdapter >> listDirection [
	
	self model isPlaceAtEnd ifTrue: [ ^ #rightToLeft ].
	self model isPlaceAtStart ifTrue: [ ^ #leftToRight ].
	
	self error: 'Undefined location to place buttons.'
]
