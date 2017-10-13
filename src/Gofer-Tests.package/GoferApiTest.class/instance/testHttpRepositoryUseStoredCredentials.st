tests - repositories
testHttpRepositoryUseStoredCredentials

	| exampleServer registry |
	[
		exampleServer := 'http://my.cool.repo.server.com/'.
		registry := MCServerRegistry uniqueInstance.
		registry on: exampleServer beUser: 'myusername' withPassword: 'mypassword'.
		
		gofer url: exampleServer.
		
		self assert: gofer repositories: { exampleServer }.
		self assert: gofer repositories first user equals: 'myusername'
	] ensure: [
		registry removeCredentialsFor: exampleServer ].
	