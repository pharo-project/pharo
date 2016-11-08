settings
minDistanceBeforeDetectionSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #minDistanceBeforeDetection)
		target: self;
		label: 'Minimal fingers distance crossed (%)';
		description:
				'The distance, in percentage of the screen, that a finger of both fingers needs to travel before event detection';
		parent: #pinchGesture