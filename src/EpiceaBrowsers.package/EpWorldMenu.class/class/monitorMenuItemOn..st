world menu
monitorMenuItemOn: aBuilder 
	<worldMenu> 
	
	(aBuilder item: EpMonitorModel shortTitle)
		parent: self title;
		action: [ EpMonitorModel open ];
		icon: EpLogBrowser defaultIcon;
		order: 1.