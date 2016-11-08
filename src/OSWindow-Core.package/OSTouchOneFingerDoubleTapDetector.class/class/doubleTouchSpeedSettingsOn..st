settings
doubleTouchSpeedSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #doubleTouchSpeed)
		target: self;
		label: 'Max time between tap (ms)';
		description:
				'The time between two touch to send double touch (in milliseconds)';
		parent: #doubleTapGesture