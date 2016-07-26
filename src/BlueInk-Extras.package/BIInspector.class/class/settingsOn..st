settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #blueInkFormatterExtra)
		target: self;
		parent: #blueInkFormatter;
		label: 'BlueInk Formatting Editor';
		description: 'Open the BlueInk inspector to edit the formatting options';
		dialog: [ self dialogOpenBIInspector ]

	