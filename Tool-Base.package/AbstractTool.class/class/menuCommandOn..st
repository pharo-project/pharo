world menu
menuCommandOn: aBuilder  
	<worldMenu>
	(aBuilder item: #Tools)
		order: 1.0;
		target: self;
		icon: (Smalltalk ui icons iconNamed: #toolsIcon). 