"
I am the adapter used to bridge an IconListPresenter and a PluggableIconListMorph
"
Class {
	#name : #MorphicIconListAdapter,
	#superclass : #MorphicListAdapter,
	#category : #'Spec-MorphicAdapters'
}

{ #category : #factory }
MorphicIconListAdapter >> buildWidget [

	^ PluggableIconListMorph new
		    	model: self ;
			getListSizeSelector: #listSize ;
			getListElementSelector: #listElementAt: ;
			getIndexSelector: #getIndex ;
			setIndexSelector: #setIndex: ;
			getSelectionListSelector:	#getSelectionStateFor: ;
			setSelectionListSelector:	#setSelectionStateFor:at: ;
			getIconSelector: #getIconFor: ;
			resetListSelector:	#resetListSelection ; 
			getMenuSelector:	#menu:shifted: ;
			setMultipleSelection:	self multiSelection;
			wrapSelector: 	#wrapItem:index: ;
			dragEnabled:	self dragEnabled ;
			dropEnabled:	self dropEnabled ; 	
			setBalloonText: self help; 
			hResizing: 	#spaceFill;
			vResizing: 	#spaceFill;
			yourself
]

{ #category : #'widget API' }
MorphicIconListAdapter >> getIconFor: anItem [

	^ self model getIconFor: anItem
]
