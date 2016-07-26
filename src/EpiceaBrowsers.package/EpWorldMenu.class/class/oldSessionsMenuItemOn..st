world menu
oldSessionsMenuItemOn: aBuilder 
	<worldMenu> 

	(aBuilder item: EpSessionBrowser shortTitle)
		parent: self title;
		action: [ EpSessionBrowser open ]; 
		icon: EpSessionBrowser icon;
		order: 2