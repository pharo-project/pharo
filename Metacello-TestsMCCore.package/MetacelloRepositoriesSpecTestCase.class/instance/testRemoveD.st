tests
testRemoveD
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