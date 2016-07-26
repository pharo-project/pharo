settings
persistentDirectionSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #persistentDirection)
		target: self;
		label: 'Persitent scrolling' translated;
		description:
				'Should the scroll be keeping his direction after started?' translated;
		parent: #scrollGesture