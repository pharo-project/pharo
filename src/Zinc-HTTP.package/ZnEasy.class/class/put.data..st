operations
put: urlObject data: entity
	^ self client
		url: urlObject;
		entity: entity;
		put;
		response	