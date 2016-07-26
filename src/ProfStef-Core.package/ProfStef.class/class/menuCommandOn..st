zen
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'Pharo Zen')
		parent: #Help;
		action: [ ProfStef openPharoZenWorkspace ];
		icon: ((Smalltalk ui icons iconNamed: #pharoIcon) scaledToSize: 16@16)