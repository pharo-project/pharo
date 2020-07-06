"
I'm the adapter used to bridge a ToolBarPresenter and its morphic representation.
"
Class {
	#name : #SpMorphicToolbarAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicToolbarAdapter >> addModelTo: toolBarMorph [

	toolBarMorph displayMode: self presenter displayMode. 
	toolBarMorph height: self presenter displayMode height.
	toolBarMorph setBalloonText: self presenter help.

	self presenter whenItemsChangeDo: [ 
		self updateItems: toolBarMorph ].

	self updateItems: toolBarMorph
]

{ #category : #factory }
SpMorphicToolbarAdapter >> buildWidget [
	| toolBarMorph |
	
	toolBarMorph := SpToolbarMorph new.
	self addModelTo: toolBarMorph.
	
	^ toolBarMorph
]

{ #category : #factory }
SpMorphicToolbarAdapter >> configureItem: itemModel morph: itemMorph toolBar: toolBarMorph [
	
	itemMorph 
		font: toolBarMorph toolbarItemFont;
		width: toolBarMorph toolbarItemSize.
	
	toolBarMorph displayMode 
		configureMorph: itemMorph 
		item: itemModel.
		
	^ itemMorph
]

{ #category : #testing }
SpMorphicToolbarAdapter >> hasButtonWithLabel: aString [

	^ (self widget leftPanel submorphs , self widget rightPanel submorphs)
		detect: [ :e | (e isKindOf: SpToolbarButtonMorph) and: [ e label = aString ] ]
		ifFound: [ true ]
		ifNone: [ false ]
]

{ #category : #testing }
SpMorphicToolbarAdapter >> numberOfItems [
	
	^ self widget leftPanel submorphs size + self widget rightPanel submorphs size
]

{ #category : #factory }
SpMorphicToolbarAdapter >> updateItems: toolBarMorph [

	toolBarMorph clearItems.

	self presenter leftItems do: [ :each | 
		toolBarMorph 
			addItem: (self 
				configureItem: each 
				morph: each buildWithSpec 
				toolBar: toolBarMorph)
			position: SpToolbarItemPosition left ].

	self presenter rightItems do: [ :each | 
		toolBarMorph 
			addItem: (self 
				configureItem: each 
				morph: each buildWithSpec 
				toolBar: toolBarMorph)
			position: SpToolbarItemPosition right ]
]
