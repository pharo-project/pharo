Class {
	#name : #MetacelloRepositorySpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsMCCore'
}

{ #category : #accessing }
MetacelloRepositorySpecTestCase >> projectClass [
    ^ MetacelloMCProject
]

{ #category : #tests }
MetacelloRepositorySpecTestCase >> testRepositoryMergeSpec [
	| repositoryA repositoryB repository |
	repositoryA := self repositorySpec
		description: '/opt/gemstone/repository';
		username: 'dkh';
		password: 'password';
		type: 'directory';
		yourself.
	repositoryB := self repositorySpec
		description: 'http://example.com/repository';
		password: 'secret';
		yourself.
	repository := repositoryA mergeSpec: repositoryB.
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'secret'.
	self assert: repository type equals: 'http'
]

{ #category : #tests }
MetacelloRepositorySpecTestCase >> testRepositorySpec [
	| repository |
	repository := self repositorySpec
		description: '/opt/gemstone/repository';
		username: 'dkh';
		password: 'password';
		type: 'directory';
		yourself.
	self assert: repository name equals: repository description.
	self assert: repository description equals: '/opt/gemstone/repository'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'.
	self assert: repository type equals: 'directory'.
	repository := self repositorySpec
		description: '/opt/gemstone/repository';
		yourself.
	self assert: repository description equals: '/opt/gemstone/repository'.
	self assert: repository type equals: 'directory'.
	repository := self repositorySpec
		description: 'http://example.com/repository';
		yourself.
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'
]
