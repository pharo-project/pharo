signalling
signalUrl: aUrl username: username password: password
	^ self new 
		url: aUrl;
		username: username;
		password: password;
		signal