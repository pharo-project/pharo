instance creation
username: username password: password realm: realm
	^ self new
		atUsername: username putPassword: password;
		realm: realm;
		yourself