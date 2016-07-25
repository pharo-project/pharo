*System-Settings-Browser
startupGeneralPrefererencesFolderMenuOn: aBuilder
	<worldMenu>
	(aBuilder item: #SystemStartupFolder)
		action: [ self preferencesGeneralFolder inspect ];
		label: 'General Preferences folder';
		parent: #SystemStartup;
		order: 3;
		help: 'Open the folder with general preferences';
		icon: (Smalltalk ui icons iconNamed: #smallOpenIcon)