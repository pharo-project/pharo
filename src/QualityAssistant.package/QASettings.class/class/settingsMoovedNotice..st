settings-definition
settingsMoovedNotice: aBuilder
	<systemsettings>

	(aBuilder group: #'resetRules')
		order: 10;
		target: self;
		label: 'Settings moved to the related categories.';
		parent: #qualityAssistant;
		description: 'Settings for QualityAssistant plugins were moved to the categories where these plugins appear. For example the nautilus plugin setting is in the Nautilus group.';
		dialog: [ (self iconNamed: #warning) asMorph ]