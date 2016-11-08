settings
minAngleBeforeDetectionSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #minAngleBeforeDetection)
		target: self;
		label: 'Minimal angle (degrees)';
		description:
				'Minimal angle, in degrees, that the fingers have to rotate before detection';
		parent: #rotationGesture