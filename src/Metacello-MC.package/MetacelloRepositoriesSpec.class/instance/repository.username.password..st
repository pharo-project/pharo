actions
repository: description username: username password: password

	| spec |
	spec := 
		(self project repositorySpec)
			description: description;
			username: username;
			password: password;
			yourself.
	self addMember: 
		(self addMember 
			name: spec name;
			spec: spec;
			yourself)