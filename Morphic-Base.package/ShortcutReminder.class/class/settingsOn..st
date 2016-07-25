settings
settingsOn: aBuilder 
	<systemsettings>
	(aBuilder setting: #'Shortcut Reminder') 
		target: self;
		parent: #appearance;
		description: 'Shortcut reminder is a reminding tool which pops up when a menu entry which includes a keyboard shortcut is invoked by the mouse instead';
		selector: #enabled;
		icon: (Smalltalk ui icons iconNamed: #smallConfigurationIcon);
		with: [
			self defaultReminder customSettingsOn: aBuilder
		]