world menu
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'Test Runner')
		parent: #MostUsedTools;
		action: [ Smalltalk tools openTestRunner ];
		order: 0.5;
		keyText: 'o, u';
		icon: self taskbarIcon