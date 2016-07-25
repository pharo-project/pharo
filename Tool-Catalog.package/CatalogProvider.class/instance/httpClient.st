private
httpClient
	^ ZnClient new
		systemPolicy;
		beOneShot;
		timeout: 5;
		setAcceptEncodingGzip;
		accept: 'text/json';
		url: CatalogSettings catalogSourceURL;
		contentReader: [ :entity | STON fromString: entity contents ];
		yourself