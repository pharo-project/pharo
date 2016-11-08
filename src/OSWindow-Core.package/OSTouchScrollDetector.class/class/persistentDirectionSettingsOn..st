settings
persistentDirectionSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #persistentDirection)
		target: self;
		label: 'Persitent scrolling';
		description:
				'Should the scroll be keeping his direction after started?';
		parent: #scrollGesture