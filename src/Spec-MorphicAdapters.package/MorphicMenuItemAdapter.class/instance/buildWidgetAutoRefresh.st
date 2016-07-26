factory
buildWidgetAutoRefresh
	^ UpdatingMenuItemMorph new
		target: self;
		balloonTextSelector: #balloonText;
		subMenuSelector: #subMenu;
		nameSelector: #name;
		selector: #performMenuActionWith:;
		argumentsBlock: self actionArguments;
		getStateSelector: #state;
		iconSelector: #icon;
		enablementSelector: #enabled;
		keyText: self shortcutText;
		yourself