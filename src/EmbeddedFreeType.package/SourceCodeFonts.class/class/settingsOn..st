settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #useSourceCode)
		parent: #appearance;
		order: 4;
		target: self;
		icon: (Smalltalk ui icons iconNamed: #smallConfigurationIcon);
		label: 'Source Code Fonts';
		description: 'Use Source Code Pro Fonts';
		precondition: [ FT2Library current notNil ];
		dialog: [ self fontSourceCodeRow ]