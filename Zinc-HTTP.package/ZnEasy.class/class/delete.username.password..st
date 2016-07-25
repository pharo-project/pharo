operations
delete: urlObject username: username password: password
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		delete;
		response
	