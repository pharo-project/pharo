settings
doubleTouchSpeedSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #doubleTouchSpeed)
		target: self;
		label: 'Max time between tap (ms)' translated;
		description:
				'The time between two touch to send double touch (in milliseconds)' translated;
		parent: #doubleTapGesture