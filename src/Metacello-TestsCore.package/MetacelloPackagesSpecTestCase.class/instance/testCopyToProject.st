tests
testCopyToProject
    "used by MetacelloAbstractVersionConstructor>>project:copyFrom:with:"

    | packages project referenceSpec |
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
    project := self projectSpec
        name: 'ProjectCopy';
        yourself.
    referenceSpec := self project projectReferenceSpec
        name: 'ProjectCopy';
        projectReference: project;
        yourself.
    packages copy: 'Project' to: referenceSpec.
    project := (packages packageNamed: 'ProjectCopy' ifAbsent: [ self assert: false ]) referencedSpec.
    self assert: project name = 'ProjectCopy'.
    self assert: project className = 'ConfigurationOfProjectA'.
    self assert: project versionString = #'stable'.
    self assert: project operator == #'>='.
    self assert: project loads = #('MyPackage' 'MyTests').
    self assert: project preLoadDoIt value == #'preLoadDoItB'.
    self assert: project postLoadDoIt value == #'postLoadDoItB'