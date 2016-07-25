world menu
mainMenuItemOn: aBuilder
	<worldMenu> 

	(aBuilder item: self title)
		parent: #Tools;
		icon: self mainIcon;
		order: 0.5.