instance creation
unauthorized: authString entity: entity
	^ (self statusLine: ZnStatusLine unauthorized)
		setWWWAuthenticate: authString;
		entity: entity;
		yourself