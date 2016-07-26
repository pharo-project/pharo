settings
appearanceSettingsOn: aBuilder 
	<systemsettings>
	(aBuilder group: #appearance)
		label: 'Appearance' ;  
		description: 'All settings concerned with the look''n feel of your system';
		with: [
			(aBuilder pickOne: #uiThemeClass)
				label: 'User interface theme';
				target: self;
				default: UITheme standardThemeClass;
				order: 1;
				domainValues: self uiThemeClassChoices.
			self morphicSettingsOn: aBuilder.
			self desktopSettingsOn: aBuilder.
			self soundSettingsOn: aBuilder]