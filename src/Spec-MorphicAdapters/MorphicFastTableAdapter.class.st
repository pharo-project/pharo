"
I am the adapter used to bridge an FastTablePresenter and a FTPluggableIconListMorphAdaptor
"
Class {
	#name : #MorphicFastTableAdapter,
	#superclass : #MorphicListAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicFastTableAdapter >> buildWidget [
	^ FTPluggableIconListMorphAdaptor new
		  model: self model;
			hideColumnHeaders;
			beResizable;
			columns: self model columns;
			getListSizeSelector: #listSize ;
			getListElementSelector: #listElementAt: ;
			getIndexSelector: #getIndex ;
			setIndexSelector: #setIndex: ;
			getSelectionListSelector:	#getSelectionStateFor: ;
			setSelectionListSelector:	#setSelectionStateFor:at: ;
			getIconSelector: #getIconFor: ;
			resetListSelector:	#resetListSelection ; 
			getMenuSelector:	#menu:shifted: ;
			setMultipleSelection: self multiSelection;
			doubleClickSelector: #doubleClick: ;
			basicWrapSelector: #wrapItem: ;
			dragEnabled:	self dragEnabled ;
			dropEnabled:	self dropEnabled ; 	
			setBalloonText: self help; 
			hResizing: 	#spaceFill;
			vResizing: 	#spaceFill;
			selectIndex: 1;
			enableFilter: FTSubstringFilter;			
			explicitFunction;
			yourself
]

{ #category : #'as yet unclassified' }
MorphicFastTableAdapter >> doubleClick: event [

	^ self model doubleClick: event
]

{ #category : #'widget API' }
MorphicFastTableAdapter >> getIconFor: anItem [

	^ self model getIconFor: anItem
]

{ #category : #'as yet unclassified' }
MorphicFastTableAdapter >> wrapItem: anObject [

	^ self model wrapItem: anObject
]
