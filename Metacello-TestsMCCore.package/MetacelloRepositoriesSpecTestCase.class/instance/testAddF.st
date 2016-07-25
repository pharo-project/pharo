tests
testAddF
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