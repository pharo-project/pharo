instance creation
ok: entity
	^ (self statusLine: ZnStatusLine ok)
		entity: entity;
		yourself