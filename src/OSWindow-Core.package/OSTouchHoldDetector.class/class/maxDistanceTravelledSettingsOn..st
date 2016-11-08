settings
maxDistanceTravelledSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #maxDistanceTravelled)
		target: self;
		label: 'Detection area (%)';
		description:
				'The area to consider the finger to be still';
		parent: #holdGesture