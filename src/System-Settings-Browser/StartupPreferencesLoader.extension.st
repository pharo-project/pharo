Extension { #name : 'StartupPreferencesLoader' }

{ #category : '*System-Settings-Browser' }
StartupPreferencesLoader class >> defineStartUpActionMenuOn: aBuilder [
	<worldMenu>
	
	(aBuilder item: #SystemStartupCreator)
		action: [ self default 
						addTemplateForStartupInDirectory: self preferencesGeneralFolder named: 'PharoPreferences.st' ];
		label: 'Define a preference file';
		parent: #SystemStartup;
		order: 2;
		help: 'Propose and define a startup action file for the current version of Pharo.';
		iconName: #scriptManager.
	aBuilder withSeparatorAfter
]

{ #category : '*System-Settings-Browser' }
StartupPreferencesLoader class >> startupGeneralPrefererencesFolderMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #SystemStartupFolder)
		action: [ self preferencesGeneralFolder inspect ];
		label: 'General Preferences folder';
		parent: #SystemStartup;
		order: 3;
		help: 'Open the folder with general preferences.';
		iconName: #smallOpen
]

{ #category : '*System-Settings-Browser' }
StartupPreferencesLoader class >> startupLoaderMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #SystemStartupLoader)
		action: [ self default loadFromDefaultLocations ];
		label: 'Run startup scripts';
		parent: #SystemStartup;
		order: 1;
		help: 'Run startup scripts.';
		iconName: #scriptManager.

]

{ #category : '*System-Settings-Browser' }
StartupPreferencesLoader class >> startupPreferencesVersionFolderMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #SystemStartupFolder)
		action: [ self preferencesVersionFolder
				ensureCreateDirectory;
				inspect ];
		label: 'Version Preferences folder';
		parent: #SystemStartup;
		order: 2;
		help: 'Open the preferences per version folder.';
		iconName: #smallOpen
]

{ #category : '*System-Settings-Browser' }
StartupPreferencesLoader class >> systemStartupMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #SystemStartup)
		label: 'Startup';
		parent: #Startup;
		order: 2;
		help: 'System startup related.';
		iconName: #scriptManager
]
