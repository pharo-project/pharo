ui
menuCommandOn: aBuilder
	<worldMenu>

	(aBuilder item: #VersionnerSpecBrowser)
		order: 0.21;
		parent: #Tools;
		label: 'Versionner';
		icon: (Smalltalk ui icons iconNamed: #databaseIcon);
		action: [ VersionnerSpecBrowser open ]