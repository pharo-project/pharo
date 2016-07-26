settings
catalogSettingsOn: aBuilder  
	<systemsettings>  
	(aBuilder group: #catalog)
		label: 'Catalog';
		parent: #tools;
		description: 'All Catalog and CatalogBrowser settings';
		with: [ 
			(aBuilder setting: #displayCatalogProjectsInSpotter)
						target: self;
						label: 'Display catalog projects in Spotter';
						default: true.
			(aBuilder setting: #catalogSourceURL)
				target: self;
				label: 'Source URL' ;
				ghostHelp: 'http://pathTo.url/json';
				description: 'Enter the source URL used to retrieve catalog information'			
		]