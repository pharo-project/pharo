ui
menuCommandOn: aBuilder
	<worldMenu>

	(aBuilder item: #VersionnerSpecBrowser)
		order: 0.21;
		parent: #Tools;
		label: 'Versionner';
		icon: (self iconNamed: #databaseIcon);
		action: [ VersionnerSpecBrowser open ]