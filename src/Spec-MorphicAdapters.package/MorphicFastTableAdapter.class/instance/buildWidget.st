factory
buildWidget

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
			doubleClickSelector: #doubleClick: ;
			basicWrapSelector: #wrapItem: ;
			dragEnabled:	self dragEnabled ;
			dropEnabled:	self dropEnabled ; 	
			setBalloonText: self help; 
			hResizing: 	#spaceFill;
			vResizing: 	#spaceFill;
			selectRowIndex: 1;
			enableFilter: FTSubstringFilter;			
			explicitFunction;
			yourself