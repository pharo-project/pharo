tests
testCopyTo
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