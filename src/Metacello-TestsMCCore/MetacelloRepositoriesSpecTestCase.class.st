Class {
	#name : #MetacelloRepositoriesSpecTestCase,
	#superclass : #MetacelloSpecTestCase,
	#category : #'Metacello-TestsMCCore'
}

{ #category : #accessing }
MetacelloRepositoriesSpecTestCase >> projectClass [
    ^ MetacelloMCProject
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testAddA [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories
        add:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'dkh';
                        password: 'password';
                        yourself);
        add:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'DaleHenrichs';
                        password: 'secret';
                        yourself).
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    self assert: repository username = 'DaleHenrichs'.
    self assert: repository password = 'secret'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testAddB [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories
        add:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'dkh';
                        password: 'password';
                        yourself);
        add: '/opt/gemstone/repository'.
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    self assert: repository username = 'dkh'.
    self assert: repository password = 'password'.
    repository := repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = '/opt/gemstone/repository'.
    self assert: repository type = 'directory'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testAddC [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories add: #('http://example.com/repository' '/opt/gemstone/repository').
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    repository := repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = '/opt/gemstone/repository'.
    self assert: repository type = 'directory'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testAddE [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories
        repository: 'http://example.com/repository' username: 'dkh' password: 'password';
        repository: '/opt/gemstone/repository'.
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    repository := repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = '/opt/gemstone/repository'.
    self assert: repository type = 'directory'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testAddF [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories
        repository:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'dkh';
                        password: 'password';
                        yourself);
        repository:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'DaleHenrichs';
                        password: 'secret';
                        yourself).
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    self assert: repository username = 'DaleHenrichs'.
    self assert: repository password = 'secret'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testCopyTo [
    | repositories |
    repositories := self repositoriesSpec.
    repositories
        add:
            (self repositorySpec
                description: 'http://example.com/repository';
                username: 'dkh';
                password: 'password';
                yourself).
    self
        should: [ 
            repositories
                copy: 'http://example.com/repository'
                to:
                    (self repositorySpec
                        description: 'http://example.com/alternate/repository';
                        yourself) ]
        raise: Error
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testMergeA [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories
        add:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'dkh';
                        password: 'password';
                        yourself);
        merge:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'DaleHenrichs';
                        password: 'secret';
                        yourself).
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    self assert: repository username = 'DaleHenrichs'.
    self assert: repository password = 'secret'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testMergeB [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories
        add:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'dkh';
                        password: 'password';
                        yourself);
        merge: 'http://example.com/repository'.
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    self assert: repository username = 'dkh'.
    self assert: repository password = 'password'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testMergeC [
    | repositories repository |
    repositories := self repositoriesSpec.
    repositories
        add:
                (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'dkh';
                        password: 'password';
                        yourself);
        merge:
                {(self repositorySpec
                        description: 'http://example.com/repository';
                        password: 'secret';
                        yourself).
                    (self repositorySpec
                        description: 'http://example.com/repository';
                        username: 'DaleHenrichs';
                        yourself)}.
    repository := repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository description = 'http://example.com/repository'.
    self assert: repository type = 'http'.
    self assert: repository username = 'DaleHenrichs'.
    self assert: repository password = 'secret'
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testRemoveA [
    | repositories removed |
    repositories := self repositoriesSpec.
    repositories
        add:
            (self repositorySpec
                description: 'http://example.com/repository';
                username: 'dkh';
                password: 'password';
                yourself).
    repositories
        remove:
            (self repositorySpec
                description: 'http://example.com/repository';
                yourself).
    removed := false.
    repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testRemoveB [
    | repositories removed |
    repositories := self repositoriesSpec.
    repositories
        add:
            (self repositorySpec
                description: 'http://example.com/repository';
                username: 'dkh';
                password: 'password';
                yourself).
    repositories
        remove:
            {(self repositorySpec
                description: 'http://example.com/repository';
                yourself)}.
    removed := false.
    repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testRemoveC [
    | repositories removed |
    repositories := self repositoriesSpec.
    repositories
        add:
            (self repositorySpec
                description: 'http://example.com/repository';
                username: 'dkh';
                password: 'password';
                yourself).
    repositories remove: {'http://example.com/repository'}.
    removed := false.
    repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
    self assert: removed
]

{ #category : #tests }
MetacelloRepositoriesSpecTestCase >> testRemoveD [
    | repositories removed |
    repositories := self repositoriesSpec.
    repositories
        add:
            (self repositorySpec
                description: 'http://example.com/repository';
                username: 'dkh';
                password: 'password';
                yourself).
    repositories remove: 'http://example.com/repository'.
    removed := false.
    repositories map at: 'http://example.com/repository' ifAbsent: [ removed := true ].
    self assert: removed
]
