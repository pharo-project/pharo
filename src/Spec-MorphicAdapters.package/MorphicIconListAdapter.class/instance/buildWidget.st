factory
buildWidget

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