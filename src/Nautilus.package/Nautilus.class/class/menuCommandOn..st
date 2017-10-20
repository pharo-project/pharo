world menu
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'System Browser')
		parent: #MostUsedTools;
		action: [ Smalltalk tools openClassBrowser ];
		order: 0.1;
		keyText: 'o, b';
		icon: Smalltalk tools browser taskbarIcon