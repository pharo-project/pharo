settings
settingsOn: aBuilder 
	<systemsettings>

	(aBuilder setting: #current)
		parent: #appearance;
		order: 2;
		label: 'Icon Set';
		target: self;
		dialog: [ self createSettingRow ]