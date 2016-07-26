world menu
currentSessionMenuItemOn: aBuilder 
	<worldMenu> 
	
	(aBuilder item: 'Current Session')
		parent: self title;
		action: [ EpLogBrowser open ];
		icon: EpLogBrowser defaultIcon;
		order: 1.