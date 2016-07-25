tests
testPackageMergeSpec
    | packageA packageB package repository |
    packageA := self packageSpec
        name: 'Package';
        name: 'Package';
        requires: 'AnotherPackage';
        includes: 'IncludedPackage';
        answers: #(#('preload' 'preload answer') #('postload' 'postload answer'));
        file: 'Package-dkh.1';
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        repository: 'http://example.com/repository' username: 'dkh' password: 'password';
        repository: '/opt/gemstone/repository';
        yourself.
    packageB := self packageSpec
        name: 'Package';
        requires: 'AndAnotherPackage';
        includes: 'AndIncludedPackage';
        answers: #(#('xpostload' 'xpostload answer'));
        file: 'Package-dkh.2';
        repository: 'http://example.com/repository' username: 'DaleHenrichs' password: 'secret';
        repository: '/opt/gemstone/repo';
        yourself.
    package := packageA mergeSpec: packageB.
    self assert: package name = 'Package'.
    self assert: package requires = #('AnotherPackage' 'AndAnotherPackage').
    self assert: package includes = #('IncludedPackage' 'AndIncludedPackage').
    self
        assert:
            package answers = #(#('preload' 'preload answer') #('postload' 'postload answer') #('xpostload' 'xpostload answer')).
    self assert: package file = 'Package-dkh.2'.
    self assert: package preLoadDoIt value == #'preLoadDoIt'.
    self assert: package postLoadDoIt value == #'postLoadDoIt'.
    repository := package repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
    self assert: repository type = 'directory'.
    repository := package repositories map at: '/opt/gemstone/repo' ifAbsent: [ self assert: false ].
    self assert: repository type = 'directory'.
    repository := package repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository type = 'http'.
    self assert: repository username = 'DaleHenrichs'.
    self assert: repository password = 'secret'