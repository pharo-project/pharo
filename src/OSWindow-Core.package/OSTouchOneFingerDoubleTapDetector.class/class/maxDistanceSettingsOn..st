settings
maxDistanceSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #maxDistance)
		target: self;
		label: 'Max distance between tap (%)';
		description:
				'The distance tolerance to send double touch event (in percentage of the window)';
		parent: #doubleTapGesture