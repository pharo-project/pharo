tests
testProjectMergeSpec
    | projectA projectB project |
    projectA := self projectSpec
        name: 'Project';
        className: 'ConfigurationOfProject';
        versionString: '1.0';
        loads: #('MyPackage');
        preLoadDoIt: #'preLoadDoIt';
        postLoadDoIt: #'postLoadDoIt';
        yourself.
    projectB := self projectSpec
        name: 'Project';
        className: 'ConfigurationOfProjectB';
        versionString: #'stable';
        operator: #'<';
        loads: #('MyPackage' 'MyTests');
        preLoadDoIt: #'preLoadDoItB';
        postLoadDoIt: #'postLoadDoItB';
        yourself.
    project := projectA mergeSpec: projectB.
    self assert: project name = 'Project'.
    self assert: project className = 'ConfigurationOfProjectB'.
    self assert: project versionString = #'stable'.
    self assert: project operator == #'<'.
    self assert: project loads = #('MyPackage' 'MyTests').
    self assert: project preLoadDoIt value == #'preLoadDoItB'.
    self assert: project postLoadDoIt value == #'postLoadDoItB'