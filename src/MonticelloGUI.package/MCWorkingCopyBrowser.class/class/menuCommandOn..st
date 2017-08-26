world menu
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #'Monticello Browser')
		parent: #MostUsedTools;
		action: [ Smalltalk tools openMonticelloBrowser ];
		order: 0.9;
		keyText: 'o, p';
		icon: Smalltalk tools monticelloBrowser taskbarIcon