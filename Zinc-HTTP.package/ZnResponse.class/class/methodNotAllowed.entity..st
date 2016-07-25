instance creation
methodNotAllowed: request entity: entity
	^ (self statusLine: ZnStatusLine methodNotAllowed)
		entity: entity;
		yourself