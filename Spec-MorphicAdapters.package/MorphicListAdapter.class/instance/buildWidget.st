factory
buildWidget

	^ PluggableListMorph new
		model: self;
		getListSizeSelector: #listSize;
		autoDeselect: self autoDeselect;
		getIndexSelector: #getIndex;
		setIndexSelector: #setIndex:;
		getSelectionListSelector: #getSelectionStateFor:;
		setSelectionListSelector: #setSelectionStateFor:at:;
		backgroundColoringBlockOrSelector: #backgroundColorFor:at: ;
		getListElementSelector: #listElementAt: ;
		resetListSelector: #resetListSelection;
		getMenuSelector: #menu:shifted: ;
		setMultipleSelection: self multiSelection;
		wrapSelector: #wrapItem:index: ;
		setBalloonText: self help;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;	
		hResizing: #spaceFill;
		vResizing: #spaceFill