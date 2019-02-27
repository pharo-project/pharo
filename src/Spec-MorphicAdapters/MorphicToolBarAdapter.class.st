"
I'm the adapter used to bridge a ToolBarPresenter and its morphic representation.
"
Class {
	#name : #MorphicToolBarAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicToolBarAdapter >> addModelTo: toolBarMorph [

	toolBarMorph displayMode: self model displayMode. 
	toolBarMorph height: self model displayMode height.

	self model items 
		at: ToolBarItemPosition left 
		ifPresent: [ :items |
			items do: [ :each | 
				toolBarMorph addItemLeft: (self 
					configureItem: each 
					morph: each buildWithSpec 
					toolBar: toolBarMorph) ] ].

	self model items 
		at: ToolBarItemPosition right 
		ifPresent: [ :items |
			items do: [ :each | 
				toolBarMorph addItemRight: (self 
					configureItem: each 
					morph: each buildWithSpec 
					toolBar: toolBarMorph) ] ]
]

{ #category : #factory }
MorphicToolBarAdapter >> buildWidget [
	| toolBarMorph |
	
	toolBarMorph := ToolBarMorph new.
	self addModelTo: toolBarMorph.
	
	^ toolBarMorph
]

{ #category : #factory }
MorphicToolBarAdapter >> configureItem: itemModel morph: itemMorph toolBar: toolBarMorph [
	
	itemMorph 
		font: toolBarMorph toolbarItemFont;
		width: toolBarMorph toolbarItemSize.
	
	toolBarMorph displayMode 
		configureButton: itemMorph 
		item: itemModel.
		
	^ itemMorph
]
