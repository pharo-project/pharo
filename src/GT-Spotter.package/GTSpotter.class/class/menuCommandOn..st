world menu
menuCommandOn: aBuilder
	<worldMenu>
	(aBuilder item: #Spotter)
		action: [ GTSpotterGlobalShortcut openGlobalSpotter ];
		keyText: 'Shift + Enter';
		order: 0.7;
		parent: #MostUsedTools;
		iconName: #smallFindIcon