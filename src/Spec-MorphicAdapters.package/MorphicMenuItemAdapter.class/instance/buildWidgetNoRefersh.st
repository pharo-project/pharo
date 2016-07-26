factory
buildWidgetNoRefersh
	^ ToggleMenuItemMorph new
		contents: self name;
		target: self;
		selector: #performMenuActionWith:;
		subMenu: self subMenu;
		argumentsBlock: self actionArguments;
		getStateSelector: self stateSelector;
		icon: self icon;
		enablementSelector: #enabled;
		balloonText: self balloonText;
		keyText: self shortcutText;
		yourself