settings
minDistanceBeforeDetectionSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #minDistanceBeforeDetection)
		target: self;
		label: 'Minimal fingers distance crossed (%)';
		description:
				'The distance, in percentage of the screen, that both fingers needs to travel before event detection';
		parent: #scrollGesture