zen
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'Pharo Zen')
		parent: #Help;
		order: 3.0;
		action: [ self openPharoZenWorkspace ];
		icon: ((self iconNamed: #pharoIcon) scaledToSize: 16@16);
		withSeparatorAfter 