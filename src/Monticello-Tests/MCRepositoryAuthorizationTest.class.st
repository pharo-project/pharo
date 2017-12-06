Class {
	#name : #MCRepositoryAuthorizationTest,
	#superclass : #TestCase,
	#category : #'Monticello-Tests-Repository'
}

{ #category : #asserting }
MCRepositoryAuthorizationTest >> assert: registry on: serverUrlString hasUser: nameString withPassword: passwordString [ 

	registry repositoryAt: serverUrlString credentialsDo: [ :user :password |
		self assert: nameString equals: user.
		self assert: passwordString equals: password ].
]

{ #category : #accessing }
MCRepositoryAuthorizationTest >> exampleServerUrl [

	^ 'http://www.squeaksource.com'.
]

{ #category : #tests }
MCRepositoryAuthorizationTest >> testEmptyCredentials [

	| repo |
	"Although you can do this, it's easier to use #location:. See #testNoCredentials"
	repo := MCHttpRepository location: self exampleServerUrl user: '' password: ''.
	self assert: repo user equals: ''.
	self assert: repo password equals: ''.
]

{ #category : #tests }
MCRepositoryAuthorizationTest >> testNoCredentials [

	| repo |
	repo := MCHttpRepository location: self exampleServerUrl.
	self assert: repo user equals: ''.
	self assert: repo password equals: ''.
]

{ #category : #tests }
MCRepositoryAuthorizationTest >> testOverrideStoredCredentials [

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
]

{ #category : #tests }
MCRepositoryAuthorizationTest >> testStoredCredentials [

	| repo registry |
	[
		registry := MCServerRegistry uniqueInstance.
		registry on: self exampleServerUrl beUser: 'myusername' withPassword: 'mypassword'.
		
		repo := MCHttpRepository location: self exampleServerUrl.
		
		self assert: repo user equals: 'myusername'.
		self assert: repo password equals: 'mypassword'.
	] ensure: [
		registry removeCredentialsFor: self exampleServerUrl ].
]

{ #category : #tests }
MCRepositoryAuthorizationTest >> testStoredCredentialsUsesMostSpecific [

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
]
