world menu
menuCommandOn: aBuilder 
	"Add a custom menu item to the world menu"	
	<worldMenu> 
	
	(aBuilder item: #'Iceberg')
			order: 0.18; 
	      icon: Smalltalk ui icons komitterSmalltalkhubRemote;  
			parent: #'Tools';
			action: [ self open ].
