settings
minAngleBeforeDetectionSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #minAngleBeforeDetection)
		target: self;
		label: 'Minimal angle (degrees)' translated;
		description:
				'Minimal angle, in degrees, that the fingers have to rotate before detection' translated;
		parent: #rotationGesture