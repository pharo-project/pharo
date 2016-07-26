tests
testMergeProjectA
    "used by MetacelloAbstractVersionConstructor>>project:with:"

    | packages project projectReferenceSpec referenceSpec |
    packages := self packagesSpec.
    project := self projectSpec
        name: 'Project';
        className: 'ConfigurationOfProjectA';
        versionString: #'stable';
        loads: #('MyPackage' 'MyTests');
        preLoadDoIt: #'preLoadDoItB';
        postLoadDoIt: #'postLoadDoItB';
        yourself.
    referenceSpec := self project projectReferenceSpec
        name: project name;
        projectReference: project;
        yourself.
    packages add: referenceSpec.
    project := self projectSpec
        name: 'Project';
        className: 'ConfigurationOfProject';
        versionString: '1.0';
        operator: #'<';
        loads: #('MyPackage');
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    referenceSpec := self project projectReferenceSpec
        name: project name;
        projectReference: project;
        yourself.
    packages merge: referenceSpec.
    projectReferenceSpec := packages packageNamed: 'Project' ifAbsent: [ self assert: false ].
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