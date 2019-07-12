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

	self model items 
		at: SpToolBarItemPosition left 
		ifPresent: [ :items |
			items do: [ :each | 
				toolBarMorph addItemLeft: (self 
					configureItem: each 
					morph: each buildWithSpec 
					toolBar: toolBarMorph) ] ].

	self model items 
		at: SpToolBarItemPosition right 
		ifPresent: [ :items |
			items do: [ :each | 
				toolBarMorph addItemRight: (self 
					configureItem: each 
					morph: each buildWithSpec 
					toolBar: toolBarMorph) ] ]
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
