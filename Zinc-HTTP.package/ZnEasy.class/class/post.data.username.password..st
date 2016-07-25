operations
post: urlObject data: entity username: username password: password
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		entity: entity;
		post;
		response