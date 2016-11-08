settings-definition
qaNautilusPlugginSettingOn: aBuilder
	<systemsettings>

	(aBuilder setting: #nautilusPlugin)
		order: 4;
		target: self;
		label: 'Nautilus Plugin';
		description: 'Enable Nautilus plugin which shows a group with critiques and allows to preview them.';
		parent: #qualityAssistant.