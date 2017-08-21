world menu
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: Smalltalk tools workspace title asSymbol)
		parent: #MostUsedTools;
		action: [ Smalltalk tools openWorkspace ];
		order: 0.3;
		keyText: 'o, w';
		icon: Smalltalk tools workspace taskbarIcon