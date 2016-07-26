tests
testAddProjectA
    "used by MetacelloAbstractVersionConstructor>>project:overrides:"

    | packages project projectReferenceSpec |
    packages := self packagesSpec.
    packages
        add:
            (self projectSpec
                name: 'Project';
                className: 'ConfigurationOfProjectA';
                versionString: #'stable';
                loads: #('MyPackage' 'MyTests');
                preLoadDoIt: #'preLoadDoItB';
                postLoadDoIt: #'postLoadDoItB';
                yourself).
    packages
        add:
            (self projectSpec
                name: 'Project';
                className: 'ConfigurationOfProject';
                versionString: '1.0';
                operator: #'<';
                loads: #('MyPackage');
                preLoadDoIt: #'preLoadDoIt';
                postLoadDoIt: #'postLoadDoIt';
                yourself).
    projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
    self assert: projectReferenceSpec projectName = 'Project'.
    self assert: projectReferenceSpec versionString = '1.0'.
    self should: [ projectReferenceSpec includes: #() ] raise: Error.
    self should: [ projectReferenceSpec requires: #() ] raise: Error.
    self should: [ projectReferenceSpec answers: #() ] raise: Error.
    projectReferenceSpec
        projectDo: [ :prjct | self assert: projectReferenceSpec == prjct ]
        packageDo: [ :ignored | self assert: false ]
        groupDo: [ :ignored | self assert: false ].
    project := projectReferenceSpec referencedSpec.
    self assert: project name = 'Project'.
    self assert: project className = 'ConfigurationOfProject'.
    self assert: project versionString = '1.0'.
    self assert: project operator == #'<'.
    self assert: project loads = #('MyPackage').
    self assert: project preLoadDoIt value == #'preLoadDoIt'.
    self assert: project postLoadDoIt value == #'postLoadDoIt'.
    project
        projectDo: [ :prjct | self assert: project == prjct ]
        packageDo: [ :ignored | self assert: false ]
        groupDo: [ :ignored | self assert: false ]