instance creation
username: username password: password
	^ self new
		atUsername: username putPassword: password;
		yourself