tests
testPackageSpec
    | package repository |
    package := self packageSpec
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
    self assert: package name = 'Package'.
    self assert: package requires = #('AnotherPackage').
    self assert: package includes = #('IncludedPackage').
    self assert: package answers = #(#('preload' 'preload answer') #('postload' 'postload answer')).
    self assert: package file = 'Package-dkh.1'.
    self assert: package preLoadDoIt value == #'preLoadDoIt'.
    self assert: package postLoadDoIt value == #'postLoadDoIt'.
    repository := package repositories map at: '/opt/gemstone/repository' ifAbsent: [ self assert: false ].
    self assert: repository type = 'directory'.
    repository := package repositories map at: 'http://example.com/repository' ifAbsent: [ self assert: false ].
    self assert: repository type = 'http'.
    self assert: repository username = 'dkh'.
    self assert: repository password = 'password'