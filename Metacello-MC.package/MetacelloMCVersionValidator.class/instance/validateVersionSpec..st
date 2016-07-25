private
validateVersionSpec: versionSpec
  versionSpec blessing value == #'broken'
    ifTrue: [ ^ self ].
  versionSpec
    projectDo: [ :projectSpec | 
      | referencedProjectSpec |
      projectSpec versionString == nil
        ifTrue: [ 
          self
            recordValidationCriticalWarning:
              'No version specified for the project reference ' , projectSpec name printString
                , ' in version ' , versionSpec versionString printString
            versionString: versionSpec versionString
            callSite: #'validateVersionSpec:'
            reasonCode: #'noVersionSpecified' ].
      referencedProjectSpec := projectSpec referencedSpec.
      versionSpec blessing value == #'baseline'
        ifTrue: [ 
          referencedProjectSpec hasRepository
            ifTrue: [ 
              (referencedProjectSpec file beginsWith: referencedProjectSpec className)
                ifFalse: [ 
                  self
                    recordValidationCriticalWarning:
                      'The class name of project ' , referencedProjectSpec className printString
                        , ' does not match the file name '
                        , referencedProjectSpec file printString
                        , ' in version ' , versionSpec versionString printString
                    versionString: versionSpec versionString
                    callSite: #'validateVersionSpec:'
                    reasonCode: #'projectClassNameFileMismatch' ] ]
            ifFalse: [ 
              self
                recordValidationError:
                  'The specification for the project reference ' , projectSpec name printString
                    , ' in version ' , versionSpec versionString printString
                    , ' is missing the required repository field'
                versionString: versionSpec versionString
                callSite: #'validateVersionSpec:'
                reasonCode: #'incompleteProjectSpec' ] ].
      referencedProjectSpec ensureProjectLoaded
        ifTrue: [ 
          [ projectSpec version ]
            on: MetacelloVersionDoesNotExistError
            do: [ :ex | 
              | explanation |
              explanation := projectSpec versionString == nil
                ifTrue: [ 'the default version' ]
                ifFalse: [ 'version ' , projectSpec versionString printString ].
              self
                recordValidationError:
                  'Cannot resolve ' , explanation , ' for the project reference '
                    , projectSpec name printString , ' in version '
                    , versionSpec versionString printString
                versionString: versionSpec versionString
                callSite: #'validateVersionSpec:'
                reasonCode: #'cannotResolveVersion' ] ] ]
    packageDo: [ :packageSpec | 
      (packageSpec file beginsWith: packageSpec name)
        ifFalse: [ 
          self
            recordValidationCriticalWarning:
              'The name of package ' , packageSpec name printString
                , ' does not match the file name '
                , packageSpec file printString , ' in version '
                , versionSpec versionString printString
            versionString: versionSpec versionString
            callSite: #'validateVersionSpec:'
            reasonCode: #'packageNameMismatch' ].
      packageSpec file = packageSpec name
        ifTrue: [ 
          self
            recordValidationCriticalWarning:
              'No version specified for the package ' , packageSpec name printString
                , ' in version ' , versionSpec versionString printString
            versionString: versionSpec versionString
            callSite: #'validateVersionSpec:'
            reasonCode: #'noVersionSpecified' ] ]
    groupDo: [ :ignored |  ].
  self validateBaselineVersionSpec: versionSpec