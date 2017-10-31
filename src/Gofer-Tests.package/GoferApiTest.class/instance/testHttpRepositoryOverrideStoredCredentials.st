tests - repositories
testHttpRepositoryOverrideStoredCredentials

	| exampleServer registry |
	[
		exampleServer := 'http://my.cool.repo.server.com/'.
		registry := MCServerRegistry uniqueInstance.
		registry on: exampleServer beUser: 'myusername' withPassword: 'mypassword'.
		
		gofer url: exampleServer username: 'foo' password: 'bar'..
		
		self assert: gofer repositories: { exampleServer }.
		self assert: gofer repositories first user equals: 'foo'.
		self assert: registry on: exampleServer hasUser: 'myusername' withPassword: 'mypassword'.
	] ensure: [
		registry removeCredentialsFor: exampleServer ].
	