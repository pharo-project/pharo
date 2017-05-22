accessing
credentials
	^ IcePlaintextCredentials new 
		username: self username;
		password: self password;
		yourself.
