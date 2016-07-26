tests
testOverrideStoredCredentials

	| repo registry |
	[
		registry := MCServerRegistry uniqueInstance.
		registry on: self exampleServerUrl beUser: 'myusername' withPassword: 'mypassword'.
		
		repo := MCHttpRepository location: self exampleServerUrl user: 'user_this_time' password: 'password_this_time'.
		
		self assert: repo user equals: 'user_this_time'.
		self assert: repo password equals: 'password_this_time'.
		self assert: registry on: self exampleServerUrl hasUser: 'myusername' withPassword: 'mypassword'.
	] ensure: [
		registry removeCredentialsFor: self exampleServerUrl ].