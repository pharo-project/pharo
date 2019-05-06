"
I am the adapter used to bridge a MultiColumnListPresenter and a PluggableMultiColumnListMorph
"
Class {
	#name : #MorphicMultiColumnListAdapter,
	#superclass : #MorphicListAdapter,
	#category : #'Spec-Deprecated80-Adapters'
}

{ #category : #deprecation }
MorphicMultiColumnListAdapter class >> isDeprecated [
	^ true
]

{ #category : #factory }
MorphicMultiColumnListAdapter >> buildWidget [
	^ PluggableMultiColumnListMorph new
		wrapSelector: #wrapItem:index:;
		model: self;
		getListSizeSelector: #listSize;
		getIndexSelector: #getIndex;
		setIndexSelector: #setIndex:;
		getListElementSelector: #listElementAt:;
		resetListSelector: #resetListSelection;
		getMenuSelector: #menu:shifted:;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		setMultipleSelection: self multiSelection;
		setBalloonText: self help;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself
]
