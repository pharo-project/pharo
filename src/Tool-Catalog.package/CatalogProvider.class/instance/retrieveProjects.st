private
retrieveProjects
	^ ZnClient new
		systemPolicy;
		beOneShot;
		timeout: 0;
		setAcceptEncodingGzip;
		accept: 'text/json';
		url: CatalogSettings catalogSourceURL;
		contentReader: [ :entity | STON fromString: entity contents ];
		get