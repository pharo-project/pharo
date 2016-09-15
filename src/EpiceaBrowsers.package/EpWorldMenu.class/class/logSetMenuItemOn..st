world menu
logSetMenuItemOn: aBuilder 
	<worldMenu>

	(aBuilder item: EpLogSetModel shortTitle)
		parent: self title;
		action: [ EpLogSetModel open ]; 
		icon: EpLogSetModel icon;
		order: 2