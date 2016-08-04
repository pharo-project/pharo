system settings
enabledSettingOn: aBuilder

	<systemsettings>
	(aBuilder setting: #monitorEnabled)
		label: 'Logging enabled';
		description: 'The Epicea Monitor listens events in the system such as class added and method modified, and records them to the current Epicea log.';
		parent: #epicea;
		target: self.