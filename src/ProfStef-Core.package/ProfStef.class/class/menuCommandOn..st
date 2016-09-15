zen
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'Pharo Zen')
		parent: #Help;
		action: [ ProfStef openPharoZenWorkspace ];
		icon: ((self iconNamed: #pharoIcon) scaledToSize: 16@16)