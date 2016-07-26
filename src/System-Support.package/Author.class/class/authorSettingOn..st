settings
authorSettingOn: aBuilder
	<systemsettings>
	(aBuilder setting: #authorFullName)
		order: -100000;
		parent: #pharoSystem;
		label: 'Author identification';
		target: Author uniqueInstance;
		getSelector: #fullNamePerSe;
		setSelector: #fullName:;
		notInStyle;
		default: '';
		ghostHelp: 'VincentVanGogh';
		description: AuthorNameRequest messagePrompt
