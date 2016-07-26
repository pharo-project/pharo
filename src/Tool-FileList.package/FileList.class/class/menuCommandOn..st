world menu
menuCommandOn: aBuilder 
	<worldMenu> 
	(aBuilder item: #'File Browser')
		parent: #Tools;
		order: 0.43;
		action:[self open]; 
		icon: self taskbarIcon.
	aBuilder withSeparatorAfter.		