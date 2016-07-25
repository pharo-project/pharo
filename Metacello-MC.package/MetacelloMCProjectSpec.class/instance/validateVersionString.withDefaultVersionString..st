scripting
validateVersionString: issues withDefaultVersionString: defaultVersionString
    self versionString
        ifNil: [ 
            defaultVersionString
                ifNotNil: [ self versionString: defaultVersionString ]
                ifNil: [ 
                    issues
                        add:
                            (MetacelloValidationError
                                configurationClass: self projectClass
                                reasonCode: #'incompleteProjectSpec'
                                callSite:
                                    #'validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:'
                                explanation: 'version field required') ] ]