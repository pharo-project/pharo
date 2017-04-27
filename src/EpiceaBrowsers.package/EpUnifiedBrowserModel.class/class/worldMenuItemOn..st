accessing
worldMenuItemOn: aBuilder
	<worldMenu>

	(aBuilder item: 'Code Changes')
		parent: #Tools;
		action: [ self open ]; 
		icon: self taskbarIcon;
		order: 0.5