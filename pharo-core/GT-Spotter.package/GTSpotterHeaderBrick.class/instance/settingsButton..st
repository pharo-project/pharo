accessing
settingsButton: aBrick

	settingsButton ifNotNil: [self removeBrick: settingsButton].
	settingsButton := aBrick.
	
	self themer spotterThemer settingsButtonWidgetStyleFor: settingsButton.
	self themer spotterThemer settingsButtonStyleFor: settingsButton.
	
	settingsButton
		hintTitle: 'Settings';
		hintText: 'Shows the settings UI';
		action: [ self openSettingsBrowser ].
	
	self addBrickBack: settingsButton