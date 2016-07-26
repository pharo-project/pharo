settings
settingsOn: aBuilder 
	<systemsettings>
	
	(aBuilder setting: #stringUrlProvider)
		parent: #gt;
		target: self;
		description: 'The url for the script sharing service like the one from http://ws.stfx.eu' translated;
		label: 'The url for the script sharing service'
