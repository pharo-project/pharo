operations
head: urlObject username: username password: password
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		head;
		response