tests
testRemoveA
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