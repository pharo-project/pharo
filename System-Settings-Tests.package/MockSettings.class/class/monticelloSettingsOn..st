settings
monticelloSettingsOn: aBuilder  
	<mocksystemsettings>  
	(aBuilder group: #monticello)
		label: 'Mock Monticello';
		description: 'All Mock Monticello settings';
		with: [ 
			(aBuilder setting: #defaultDirectoryName)
				type: #Directory;
				target: self;
				description: 'The path of a directory where you want to start out when choosing local repository locations' ;
				label: 'Default local repository directory'.
			(aBuilder setting: #cacheDirectory)
				type: #Directory;
				target: self;
				description: 'The path of the local repository cache';
				label: 'Local cache directory'.	
			].