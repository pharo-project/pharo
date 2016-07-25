settings
waitingTimeSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #waitingTime)
		target: self;
		label: 'Time to hold (ms)' translated;
		description:
				'How long does a finger has to stay on the screen to be considered as holding' translated;
		parent: #holdGesture