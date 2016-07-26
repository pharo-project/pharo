tests
testProjectReferenceSpec
    | projectReference project |
    projectReference := self projectReferenceSpec
        name: 'Project';
        projectReference:
                (project := self projectSpec
                        name: 'Project';
                        className: 'ConfigurationOfProjectA';
                        versionString: #'stable';
                        yourself);
        yourself.
    self assert: projectReference name = 'Project'.
    self assert: projectReference projectName = 'Project'.
    self assert: projectReference projectReference == project.
    self should: [ projectReference includes: #() ] raise: Error.
    self should: [ projectReference requires: #() ] raise: Error.
    self should: [ projectReference answers: #() ] raise: Error.
    projectReference
        projectDo: [ :prjct | self assert: projectReference == prjct ]
        packageDo: [ :ignored | self assert: false ]
        groupDo: [ :ignored | self assert: false ]