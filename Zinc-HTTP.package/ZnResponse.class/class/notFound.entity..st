instance creation
notFound: url entity: entity
	^ (self statusLine: ZnStatusLine notFound)
		entity: entity;
		yourself