tests
testMCVersionSpec
  | version projectReferenceSpec group package repository |
  version := self versionSpec
    blessing: #'baseline';
    versionString: '1.0';
    repository: 'http://example.com/repository'
      username: 'dkh'
      password: 'password';
    repository: '/opt/gemstone/repository';
    yourself.
  version packages
    add:
        (self packageSpec
            name: 'Package';
            requires: 'AnotherPackage';
            includes: 'IncludedPackage';
            answers:
                #(#('preload' 'preload answer') #('postload' 'postload answer'));
            file: 'Package-dkh.1';
            preLoadDoIt: #'preLoadDoIt';
            postLoadDoIt: #'postLoadDoIt';
            yourself);
    add:
        (self packageSpec
            name: 'AnotherPackage';
            yourself);
    add:
        (self packageSpec
            name: 'IncludedPackage';
            yourself);
    add:
        (self groupSpec
            name: 'Platform';
            includes: 'Core';
            yourself);
    add:
        (self projectSpec
            name: 'Project';
            className: 'ConfigurationOfProjectA';
            versionString: #'stable';
            loads: #('MyPackage' 'MyTests');
            preLoadDoIt: #'preLoadDoItB';
            postLoadDoIt: #'postLoadDoItB';
            yourself);
    yourself.
  self assert: version blessing value = #'baseline'.
  self assert: version versionString value = '1.0'.
  repository := version repositories map
    at: '/opt/gemstone/repository'
    ifAbsent: [ self assert: false ].
  self assert: repository type = 'directory'.
  repository := version repositories map
    at: 'http://example.com/repository'
    ifAbsent: [ self assert: false ].
  self assert: repository type = 'http'.
  self assert: repository username = 'dkh'.
  self assert: repository password = 'password'.
  package := version packages
    packageNamed: 'Package'
    ifAbsent: [ self assert: false ].
  self assert: package name = 'Package'.
  group := version packages
    packageNamed: 'Platform'
    ifAbsent: [ self assert: false ].
  self assert: (group includes includes: 'Core').
  projectReferenceSpec := version packages
    packageNamed: 'Project'
    ifAbsent: [ self assert: false ].
  self assert: projectReferenceSpec projectName = 'Project'.
  self assert: projectReferenceSpec versionString = #'stable'.
  version
    projectDo: [ :prjct | prjct == projectReferenceSpec ]
    packageDo: [ :pkg | pkg == package ]
    groupDo: [ :grp | grp == group ]