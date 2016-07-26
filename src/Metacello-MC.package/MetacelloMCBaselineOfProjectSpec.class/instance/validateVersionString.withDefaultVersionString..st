scripting
validateVersionString: issues withDefaultVersionString: ignored
    self versionString
        ifNotNil: [ :vs | 
            | prj |
            prj := self project asBaselineProject.
            vs ~= prj singletonVersionName
                ifTrue: [ 
                    issues
                        add:
                            (MetacelloValidationError
                                configurationClass: self projectClass
                                reasonCode: #'invalidVersionString'
                                callSite:
                                    #'validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:'
                                explanation:
                                    'version field is incorrect, should be: ' , prj singletonVersionName printString) ] ]