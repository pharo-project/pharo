private
validateProjectCreationFrom: aConfigurationClass onError: aBlock
    [ ^ aConfigurationClass project ]
        on: Error , MetacelloValidationNotification , MetacelloErrorInProjectConstructionNotification
        do: [ :ex | 
            (ex isKindOf: MetacelloValidationNotification)
                ifTrue: [ 
                    self validationReport add: ex issue.
                    ex resume ].
            (ex isKindOf: MetacelloErrorInProjectConstructionNotification)
                ifTrue: [ 
                    self
                        recordValidationError:
                            'Error creating version: ' , ex versionString , ' error: ' , ex exception description
                                , ' to reproduce evalutate the following: ''[' , self configurationClass name asString
                                ,
                                    ' project ] on: MetacelloErrorInProjectConstructionNotification do: [:ex | ex resume: true ]'''
                        callSite: #'validateProjectCreationFrom:onError:'
                        reasonCode: #'versionCreationError'.
                    ex resume: false ].
            ^ aBlock value: ex ]