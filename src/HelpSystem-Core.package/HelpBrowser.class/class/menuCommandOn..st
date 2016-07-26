world menu
menuCommandOn: aBuilder 
	<worldMenu> 
	(aBuilder item: #'Help Browser')
		parent: #Help;
		action: [self open]; 
		icon: (Smalltalk ui icons iconNamed: #smallHelpIcon)