settings
maxDistanceTravelledSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #maxDistanceTravelled)
		target: self;
		label: 'Detection area (%)' translated;
		description:
				'The area to consider the finger to be still' translated;
		parent: #holdGesture