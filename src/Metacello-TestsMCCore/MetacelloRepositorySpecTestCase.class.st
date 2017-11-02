Class {
	#name : #MetacelloRepositorySpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : 'Metacello-TestsMCCore'
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
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository username = 'dkh'.
    self assert: repository password = 'secret'.
    self assert: repository type = 'http'
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
    self assert: repository name = repository description.
    self assert: repository description = '/opt/gemstone/repository'.
    self assert: repository username = 'dkh'.
    self assert: repository password = 'password'.
    self assert: repository type = 'directory'.
    repository := self repositorySpec
        description: '/opt/gemstone/repository';
        yourself.
    self assert: repository description = '/opt/gemstone/repository'.
    self assert: repository type = 'directory'.
    repository := self repositorySpec
        description: 'http://example.com/repository';
        yourself.
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'
]
