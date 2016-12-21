settings-definition
qaNautilusPlugginSettingOn: aBuilder
	<systemsettings>

	(aBuilder setting: #nautilusPlugin)
		target: self;
		label: 'Show critique list at the bottom?';
		description: 'Enable Nautilus plugin which shows a group with critiques and allows to preview them.';
		parent: #Nautilus.