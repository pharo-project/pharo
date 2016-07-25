tests
testStoredCredentials

	| repo registry |
	[
		registry := MCServerRegistry uniqueInstance.
		registry on: self exampleServerUrl beUser: 'myusername' withPassword: 'mypassword'.
		
		repo := MCHttpRepository location: self exampleServerUrl.
		
		self assert: repo user equals: 'myusername'.
		self assert: repo password equals: 'mypassword'.
	] ensure: [
		registry removeCredentialsFor: self exampleServerUrl ].