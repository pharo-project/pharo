menu
exploreSubMenu: aMenu

	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Explore Pointers';
				action: [ self explorePointers ] ] ]