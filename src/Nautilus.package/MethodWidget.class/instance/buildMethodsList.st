item creation
buildMethodsList
	methodsList := self class listClass new
		basicWrapSelector: #methodWrapper:;
		keystrokeSelector: #keyPressedOnElement:;
		getIconSelector: #methodIconFor:;
		resetListSelector: #resetMethodSelection;
		getListSizeSelector: #methodListSize;
		doubleClickSelector: #doubleClick:;
		autoDeselect: true;
		dragEnabled: true;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		basicGetListElementSelector: #getMethodItem:;
		model: self;
		getIndexSelector: #selectedMethodIndex;
		setIndexSelector: #selectedMethodIndex:;
		getSelectionListSelector: #methodSelectionAt:;
		setSelectionListSelector: #methodSelectionAt:put:;
		getMenuSelector: #elementsMenu:shifted:;
		beMultipleSelection.
	^ methodsList