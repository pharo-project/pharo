tests
testRepositoryMergeSpec
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