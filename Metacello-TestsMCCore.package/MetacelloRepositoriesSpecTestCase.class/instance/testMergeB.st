tests
testMergeB
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