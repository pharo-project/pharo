tests
testProjectReferenceMergeSpec
    | projectReferenceA projectReferenceB projectReference project |
    projectReferenceA := self projectReferenceSpec
        name: 'Project';
        projectReference:
                (self projectSpec
                        name: 'Project';
                        className: 'ConfigurationOfProjectA';
                        versionString: #'stable';
                        yourself);
        yourself.
    projectReferenceB := self projectReferenceSpec
        name: 'Project';
        projectReference:
                (self projectSpec
                        name: 'Project';
                        className: 'ConfigurationOfProjectB';
                        versionString: '1.0';
                        yourself);
        yourself.
    projectReference := projectReferenceA mergeSpec: projectReferenceB.
    self assert: projectReference name = 'Project'.
    self assert: projectReference projectName = 'Project'.
    project := projectReference projectReference.
    self assert: project className = 'ConfigurationOfProjectB'.
    self assert: project versionString = '1.0'