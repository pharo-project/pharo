tests
testStoredCredentialsUsesMostSpecific

	| repo registry nestedRepo |
	[
		nestedRepo := self exampleServerUrl, '/ss3'.
		registry := MCServerRegistry uniqueInstance.
		registry on: self exampleServerUrl beUser: 'myusername' withPassword: 'mypassword'.
		registry on: nestedRepo beUser: 'myss3username' withPassword: 'myss3password'.
		
		repo := MCHttpRepository location: nestedRepo.
		
		self assert: repo user equals: 'myss3username'.
		self assert: repo password equals: 'myss3password'.
	] ensure: [
		registry
			removeCredentialsFor: self exampleServerUrl;
			removeCredentialsFor: nestedRepo ].