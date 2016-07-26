instance creation
redirect: url entity: entity
	^ (self statusLine: ZnStatusLine redirect)
		setLocation: url asString;
		entity: entity;
		yourself