"
I'm the adapter used to bridge a ToolBarPresenter and its morphic representation.
"
Class {
	#name : #SpMorphicToolBarAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToolBarAdapter >> addModelTo: toolBarMorph [

	toolBarMorph displayMode: self model displayMode. 
	toolBarMorph height: self model displayMode height.
	toolBarMorph setBalloonText: self model help.

	self model whenItemsChangeDo: [self updateItems: toolBarMorph].

	self updateItems: toolBarMorph.
]

{ #category : #factory }
SpMorphicToolBarAdapter >> buildWidget [
	| toolBarMorph |
	
	toolBarMorph := SpToolBarMorph new.
	self addModelTo: toolBarMorph.
	
	^ toolBarMorph
]

{ #category : #factory }
SpMorphicToolBarAdapter >> configureItem: itemModel morph: itemMorph toolBar: toolBarMorph [
	
	itemMorph 
		font: toolBarMorph toolbarItemFont;
		width: toolBarMorph toolbarItemSize.
	
	toolBarMorph displayMode 
		configureButton: itemMorph 
		item: itemModel.
		
	^ itemMorph
]

{ #category : #testing }
SpMorphicToolBarAdapter >> hasButtonWithLabel: aString [

	^ (self widget leftPanel submorphs , self widget rightPanel submorphs)
		detect: [ :e | (e isKindOf: SpToolBarButtonMorph) and: [ e label = aString ] ]
		ifFound: [ true ]
		ifNone: [ false ]
]

{ #category : #testing }
SpMorphicToolBarAdapter >> numberOfItems [
	
	^ self widget leftPanel submorphs size + self widget rightPanel submorphs size
]

{ #category : #factory }
SpMorphicToolBarAdapter >> updateItems: toolBarMorph [

	toolBarMorph clearItems.

	self model leftItems do: [ :each | 
		toolBarMorph addItemLeft: (self 
			configureItem: each 
			morph: each buildWithSpec 
			toolBar: toolBarMorph) ].

	self model rightItems do: [ :each | 
		toolBarMorph addItemRight: (self 
			configureItem: each 
			morph: each buildWithSpec 
			toolBar: toolBarMorph) ]
]
