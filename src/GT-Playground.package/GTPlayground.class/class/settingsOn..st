settings
settingsOn: aBuilder 
	<systemsettings>
	
	(aBuilder setting: #GTPlaygroundStatus)
		target: self;
		parent: #gt;
		getSelector: #isGTPlaygroundEnabled;
		setSelector: #setGTPlaygroundEnabledStatus:; 
		label: 'GTPlayground';
		description: 'Enable or disable the GTPlayground.'.
	(aBuilder setting: #cacheDirectory)
		parent: #gt;
		type: #Directory;
		target: GTPlayBook;
		description: 'The path to the local Playground cache that stores all Playground scripts in ph files';
		label: 'Local Playground cache directory'.
	(aBuilder setting: #stashDirectory)
		parent: #gt;
		type: #Directory;
		target: GTPlayBook;
		description: 'The path to the local Playground stash that stores all named Playground scripts in ph files';
		label: 'Local Playground stash directory'