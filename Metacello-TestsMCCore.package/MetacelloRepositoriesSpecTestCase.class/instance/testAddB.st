tests
testAddB
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