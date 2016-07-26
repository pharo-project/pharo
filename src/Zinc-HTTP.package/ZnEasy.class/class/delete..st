operations
delete: urlObject
	^ self client
		url: urlObject;
		delete;
		response