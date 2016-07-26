instance creation
serverErrorWithEntity: entity
	^ (self statusLine: ZnStatusLine internalServerError)
		entity: entity;
		yourself