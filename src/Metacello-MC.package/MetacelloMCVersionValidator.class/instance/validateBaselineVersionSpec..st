validation
validateBaselineVersionSpec: versionSpec
    | projectNames packageNames groupNames versionMessage |
    self
        validateDoIts: versionSpec
        versionString: versionSpec versionString
        errorMessage: ' version ' , versionSpec versionString printString.
    projectNames := Set new.
    packageNames := Set new.
    groupNames := Set new.
    versionMessage := ' in version ' , versionSpec versionString printString.
    versionSpec
        projectDo: [ :projectSpec | 
            projectSpec resolveProjectSpec className == nil
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Missing required field (className:) for project reference ' , projectSpec name printString , ' in version '
                                , versionSpec versionString printString
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'incompleteProjectSpec' ].
            projectSpec resolveProjectSpec versionString == nil
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'Missing recommended field (versionString:) for project reference ' , projectSpec name printString , ' in version '
                                , versionSpec versionString printString
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'missingRecommendedProjectSpecField' ].
            projectSpec hasRepository
                ifTrue: [ 
                    (self recurse and: [ projectSpec versionString ~~ nil ])
                        ifTrue: [ 
                            | project |
                            projectSpec resolveProjectSpec ensureProjectLoaded.
                            project := self
                                validateProjectCreationFrom: projectSpec resolveProjectSpec projectClass
                                onError: [ :ex | 
                                    self
                                        recordValidationError: 'Error creating project reference: ' , ex description
                                        versionString: versionSpec versionString
                                        callSite: #'validateBaselineVersionSpec:'
                                        reasonCode: #'projectCreationError'.
                                    nil ].
                            project ~~ nil
                                ifTrue: [ 
                                    self validationReport
                                        addAll:
                                            (self class
                                                validateProject: project
                                                version: projectSpec versionString
                                                debug: self debug
                                                recurse: self recurse
                                                visited: self visited) ] ] ]
                ifFalse: [ 
                    self
                        recordValidationError:
                            'Missing required field (repository:) for project reference ' , projectSpec name printString , ' in version '
                                , versionSpec versionString printString
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'incompleteProjectSpec' ].
            self
                validateDoIts: projectSpec
                versionString: versionSpec versionString
                errorMessage: projectSpec name printString , versionMessage.
            (projectNames includes: projectSpec name)
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Duplicate projects named' , projectSpec name printString , versionMessage
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'duplicateNames' ]
                ifFalse: [ projectNames add: projectSpec name ] ]
        packageDo: [ :packageSpec | 
            self
                validateDoIts: packageSpec
                versionString: versionSpec versionString
                errorMessage: packageSpec name printString , versionMessage.
            (packageNames includes: packageSpec name)
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Duplicate packages named' , packageSpec name printString , versionMessage
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'duplicateNames' ]
                ifFalse: [ projectNames add: packageSpec name ] ]
        groupDo: [ :groupSpec | 
            (groupNames includes: groupSpec name)
                ifTrue: [ 
                    self
                        recordValidationError: 'Duplicate groups named' , groupSpec name printString , versionMessage
                        versionString: versionSpec versionString
                        callSite: #'validateBaselineVersionSpec:'
                        reasonCode: #'duplicateNames' ]
                ifFalse: [ projectNames add: groupSpec name ] ].
    (packageNames intersection: projectNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between packages and projects' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (groupNames intersection: projectNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between groups and projects' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (projectNames intersection: packageNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between projects and packages' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (groupNames intersection: packageNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between groups and packages' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (projectNames intersection: groupNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between projects and groups' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ].
    (packageNames intersection: groupNames) notEmpty
        ifTrue: [ 
            self
                recordValidationError: 'Names duplicated between packages and groups' , versionMessage
                versionString: versionSpec versionString
                callSite: #'validateBaselineVersionSpec:'
                reasonCode: #'shadowedNames' ]