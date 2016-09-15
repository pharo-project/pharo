system settings
debugSettingOn: aBuilder

	<systemsettings>
	(aBuilder setting: #debug)
		label: 'Debug mode';
		description: 'Logging system events can lead errors, e.g. I/O errors. This option indicates how to handle such errors.';
		parent: #epicea;
		target: self.