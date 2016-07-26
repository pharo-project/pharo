tests
testRemoveProjectB
    "used by MetacelloAbstractVersionConstructor>>removeProject:"

    | packages project projectReferenceSpec removed |
    packages := self packagesSpec.
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
    project := projectReferenceSpec referencedSpec.
    self assert: project name = 'Project'.
    self assert: project className = 'ConfigurationOfProject'.
    self assert: project versionString = '1.0'.
    self assert: project operator == #'<'.
    self assert: project loads = #('MyPackage').
    self assert: project preLoadDoIt value == #'preLoadDoIt'.
    self assert: project postLoadDoIt value == #'postLoadDoIt'.
    packages
        remove:
            {(self projectReferenceSpec
                name: 'Project';
                yourself)}.
    removed := false.
    packages packageNamed: 'Project' ifAbsent: [ removed := true ].
    self assert: removed