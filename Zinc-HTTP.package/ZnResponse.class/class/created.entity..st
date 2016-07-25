instance creation
created: url entity: entity
	^ (self statusLine: ZnStatusLine created)
		setLocation: url asString;
		entity: entity;
		yourself