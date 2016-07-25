*System-Settings-Browser
startupLoaderMenuOn: aBuilder
	<worldMenu>
	(aBuilder item: #SystemStartupLoader)
		action: [ self default loadFromDefaultLocations ];
		label: 'Run startup scripts';
		parent: #SystemStartup;
		order: 1;
		help: 'Run startup "scripts';
		icon: (Smalltalk ui icons iconNamed: #scriptManagerIcon).
	aBuilder withSeparatorAfter