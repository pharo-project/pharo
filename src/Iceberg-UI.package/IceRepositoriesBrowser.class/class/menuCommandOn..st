world menu
menuCommandOn: aBuilder 
	"Add a custom menu item to the world menu"	
	<worldMenu> 
	
	(aBuilder item: #'Iceberg')
		order: 0.18; 
		icon: self icon;  
		parent: #'Tools';
		action: [ self open ].
