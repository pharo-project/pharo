menu
menuCommandOn: aBuilder 
	<worldMenu> 
	(aBuilder item: #'Pharo Tutorials')
			parent: #Help;
			action:[ HelpBrowser openOn: self ]; 
			help: 'Browse and create Pharo tutorials'.