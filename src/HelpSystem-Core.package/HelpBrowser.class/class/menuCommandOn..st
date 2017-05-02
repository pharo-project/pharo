world menu
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'Help Browser')
		parent: #Help;
		order: 1.0;
		action: [ self open ];
		iconName: #smallHelpIcon;
		withSeparatorAfter 