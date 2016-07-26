scripting
validateForScriptLoad: aScriptEngine withDefaultVersionString: defaultVersionString withDefaultRepositoryDecription: defaultRepositoryDecription
    | issues callSite |
    issues := OrderedCollection new.
    callSite := #'validateForScriptLoad:withDefaultVersionString:withDefaultRepositoryDecription:'.
    self name
        ifNil: [ 
            issues
                add:
                    (MetacelloValidationError
                        configurationClass: self projectClass
                        reasonCode: #'incompleteProjectSpec'
                        callSite: callSite
                        explanation: 'name field required') ].
    self className
        ifNil: [ 
            issues
                add:
                    (MetacelloValidationError
                        configurationClass: self projectClass
                        reasonCode: #'incompleteProjectSpec'
                        callSite: callSite
                        explanation: 'className field required') ].
    self repositories isEmpty
        ifTrue: [ 
            defaultRepositoryDecription
                ifNotNil: [ self repository: defaultRepositoryDecription ]
                ifNil: [ 
                    issues
                        add:
                            (MetacelloValidationError
                                configurationClass: self projectClass
                                reasonCode: #'incompleteProjectSpec'
                                callSite: callSite
                                explanation: 'repository field required') ] ].
    self validateVersionString: issues withDefaultVersionString: defaultVersionString.
    ^ issues