tests
testAddE
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