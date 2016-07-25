instance creation
location: location
	^ MCServerRegistry uniqueInstance 
		repositoryAt: location credentialsDo: [ :username :password |
			(self repositoryClassFor: location) new
				location: location;
				user: username;
				password: password;
				yourself ]