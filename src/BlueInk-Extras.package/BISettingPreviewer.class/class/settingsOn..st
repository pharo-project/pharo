settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #blueInkFormatterExtra)
		target: self;
		parent: #blueInkFormatter;
		label: 'BlueInk Formatting Editor';
		description: 'Open the BlueInk setting previewer to edit the formatting options';
		dialog: [ self dialogOpenBIPreviewer ]

	