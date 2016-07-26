tests
testMCProjectSpec
    | project repository |
    project := self projectSpec.
    project
        name: 'Project';
        projectPackage: self project packageSpec;
        className: 'ConfigurationOfProject';
        repository: 'http://example.com/repository' username: 'dkh' password: 'password';
        repository: '/opt/gemstone/repository';
        yourself.
    self assert: project name = 'Project'.
    self assert: project className = 'ConfigurationOfProject'.
    self assert: project projectPackage name = project className.
    self assert: project projectPackage file = project className.
    repository := project repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
    self assert: repository type = 'directory'.
    repository := project repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository type = 'http'.
    self assert: repository username = 'dkh'.
    self assert: repository password = 'password'