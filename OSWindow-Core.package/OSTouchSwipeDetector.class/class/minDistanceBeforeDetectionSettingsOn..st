settings
minDistanceBeforeDetectionSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #minDistanceBeforeDetection)
		target: self;
		label: 'Minimal fingers distance crossed (%)' translated;
		description:
				'The distance, in percentage of the screen, that both fingers needs to travel before event detection' translated;
		parent: #swipeGesture