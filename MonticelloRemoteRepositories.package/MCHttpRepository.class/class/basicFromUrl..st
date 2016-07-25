instance creation
basicFromUrl: aZnUrl
	^ aZnUrl hasUsername
		ifFalse: [ self location: aZnUrl location ]
		ifTrue: [ (self repositoryClassFor: aZnUrl location) new
				location: aZnUrl location;
				user: aZnUrl username;
				password: aZnUrl password;
				yourself  ]