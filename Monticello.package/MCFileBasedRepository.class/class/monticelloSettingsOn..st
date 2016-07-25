settings
monticelloSettingsOn: aBuilder  
	<systemsettings>  
	(aBuilder group: #monticello)
		label: 'Monticello';
		parent: #SCM;
		description: 'All Monticello settings';
		with: [ 
			(aBuilder setting: #defaultDirectoryName)
				type: #Directory;
				target: MCDirectoryRepository;
				description: 'The path of a directory where you want to start out when choosing local repository locations' ;
				label: 'Default local repository directory'.
			(aBuilder setting: #cacheDirectory)
				type: #Directory;
				target: MCCacheRepository;
				description: 'The path of the local repository cache';
				label: 'Local cache directory'.	
				
				].