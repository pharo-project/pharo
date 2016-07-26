instance creation
badRequest: request entity: entity
	^ (self statusLine: ZnStatusLine badRequest)
		entity: entity;
		yourself