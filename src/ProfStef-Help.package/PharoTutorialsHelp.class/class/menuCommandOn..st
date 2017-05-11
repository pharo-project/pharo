menu
menuCommandOn: aBuilder 
	<worldMenu> 
	(aBuilder item: #'Pharo Tutorials')
			parent: #Help;
			order: 2.0;
			action:[ HelpBrowser openOn: self ]; 
			icon: ((self iconNamed: #pharoIcon) scaledToSize: 16@16);
			help: 'Browse and create Pharo tutorials'.