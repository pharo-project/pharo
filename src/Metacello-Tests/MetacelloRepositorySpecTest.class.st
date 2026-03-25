Class {
	#name : 'MetacelloRepositorySpecTest',
	#superclass : 'MetacelloAbstractSpecTest',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'tests' }
MetacelloRepositorySpecTest >> testRepositoryMergeSpec [

	| repositoryA repositoryB repository |
	repositoryA := MetacelloRepositorySpec new
		               description: '/opt/gemstone/repository';
		               username: 'dkh';
		               password: 'password';
		               type: 'directory';
		               yourself.
	repositoryB := MetacelloRepositorySpec new
		               description: 'http://example.com/repository';
		               password: 'secret';
		               yourself.
	repository := repositoryA mergeSpec: repositoryB.
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'secret'.
	self assert: repository type equals: 'http'
]

{ #category : 'tests' }
MetacelloRepositorySpecTest >> testRepositorySpec [

	| repository |
	repository := MetacelloRepositorySpec new
		              description: '/opt/gemstone/repository';
		              username: 'dkh';
		              password: 'password';
		              type: 'directory';
		              yourself.
	self assert: repository description equals: repository description.
	self assert: repository description equals: '/opt/gemstone/repository'.
	self assert: repository username equals: 'dkh'.
	self assert: repository password equals: 'password'.
	self assert: repository type equals: 'directory'.
	repository := MetacelloRepositorySpec new
		              description: '/opt/gemstone/repository';
		              yourself.
	self assert: repository description equals: '/opt/gemstone/repository'.
	self assert: repository type equals: 'directory'.
	repository := MetacelloRepositorySpec new
		              description: 'http://example.com/repository';
		              yourself.
	self assert: repository description equals: 'http://example.com/repository'.
	self assert: repository type equals: 'http'
]
