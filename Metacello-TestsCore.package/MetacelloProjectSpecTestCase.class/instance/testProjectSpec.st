tests
testProjectSpec
    | project |
    project := self projectSpec.
    project
        name: 'Project';
        className: 'ConfigurationOfProject';
        versionString: '1.0';
        versionString: #'stable';
        operator: #'<';
        operator: nil;
        loads: 'MyPackage';
        loads: #('MyPackage' 'MyTests');
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    self assert: project name = 'Project'.
    self assert: project className = 'ConfigurationOfProject'.
    self assert: project versionString = #'stable'.
    self assert: project operator == #'>='.
    self assert: project loads = #('MyPackage' 'MyTests').
    self assert: project preLoadDoIt value == #'preLoadDoIt'.
    self assert: project postLoadDoIt value == #'postLoadDoIt'.
    self should: [ project includes: #() ] raise: Error.
    self should: [ project requires: #() ] raise: Error.
    self should: [ project answers: #() ] raise: Error.
    project
        projectDo: [ :prjct | self assert: project == prjct ]
        packageDo: [ :ignored | self assert: false ]
        groupDo: [ :ignored | self assert: false ].
    self should: [ project preLoadDoIt: '' ] raise: Error.
    self should: [ project postLoadDoIt: '' ] raise: Error