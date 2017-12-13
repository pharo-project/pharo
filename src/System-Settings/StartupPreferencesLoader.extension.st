Extension { #name : #StartupPreferencesLoader }

{ #category : #'*System-Settings-Browser' }
StartupPreferencesLoader class >> startupGeneralPrefererencesFolderMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #SystemStartupFolder)
		action: [ self preferencesGeneralFolder inspect ];
		label: 'General Preferences folder';
		parent: #SystemStartup;
		order: 3;
		help: 'Open the folder with general preferences.';
		iconName: #smallOpenIcon
]

{ #category : #'*System-Settings-Browser' }
StartupPreferencesLoader class >> startupLoaderMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #SystemStartupLoader)
		action: [ self default loadFromDefaultLocations ];
		label: 'Run startup scripts';
		parent: #SystemStartup;
		order: 1;
		help: 'Run startup scripts.';
		iconName: #scriptManagerIcon.
	aBuilder withSeparatorAfter
]

{ #category : #'*System-Settings-Browser' }
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
		iconName: #smallOpenIcon
]

{ #category : #'*System-Settings-Browser' }
StartupPreferencesLoader class >> systemStartupMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #SystemStartup)
		label: 'Startup';
		parent: #System;
		order: 2;
		help: 'System startup related.';
		iconName: #scriptManagerIcon
]
