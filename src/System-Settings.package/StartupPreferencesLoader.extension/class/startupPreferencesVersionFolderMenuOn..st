*System-Settings-Browser
startupPreferencesVersionFolderMenuOn: aBuilder
	<worldMenu>
	(aBuilder item: #SystemStartupFolder)
		action: [ self preferencesVersionFolder
				ensureCreateDirectory;
				inspect ];
		label: 'Version Preferences folder';
		parent: #SystemStartup;
		order: 2;
		help: 'Open the preferences per version folder';
		iconName: #smallOpenIcon