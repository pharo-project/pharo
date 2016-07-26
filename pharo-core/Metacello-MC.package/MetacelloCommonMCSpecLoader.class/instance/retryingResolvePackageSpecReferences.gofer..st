private
retryingResolvePackageSpecReferences: packageSpec gofer: gofer
    | retryCount references repositoryError |
    retryCount := 0.
    references := #().
    [ references isEmpty and: [ retryCount < 3 ] ]
        whileTrue: [ 
            retryCount > 0
                ifTrue: [ 
                    Transcript
                        cr;
                        show: '...RETRY->' , packageSpec file ].
            references := [ self resolvePackageSpecReferences: packageSpec gofer: gofer ]
                on: Error , GoferRepositoryError
                do: [ :ex | 
                    self class retryPackageResolution
                        ifFalse: [ ex pass ].
                    retryCount >= 2
                        ifTrue: [ 
                            (ex isKindOf: GoferRepositoryError)
                                ifTrue: [ 
                                    "ignore repository errors at this point, in case an alternate repository is in the list "
                                    Transcript
                                        cr;
                                        show: 'gofer repository error: ' , ex description printString , '...ignoring'.
                                    repositoryError := ex.
                                    ex resume: #() ]
                                ifFalse: [ ex pass ] ].
                    ex return: #() ].
            retryCount := retryCount + 1 ].
    references isEmpty
        ifTrue: [ 
            Transcript
                cr;
                show: '...FAILED->' , packageSpec file.
            (MetacelloPackageSpecResolutionError new
                packageSpec: packageSpec;
                repositories: gofer repositories;
                repositoryError: repositoryError;
                yourself) signal ].
    ^ references