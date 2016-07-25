factory
buildWidget
	^ PluggableMultiColumnListMorph new
		wrapSelector: #wrapItem:index:;
		model: self;
		getListSizeSelector: #listSize;
		getIndexSelector: #getIndex;
		setIndexSelector: #setIndex:;
		getSelectionListSelector: #getSelectionStateFor:;
		setSelectionListSelector: #setSelectionStateFor:at:;
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