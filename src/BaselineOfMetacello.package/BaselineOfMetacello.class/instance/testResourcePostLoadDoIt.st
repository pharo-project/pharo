doits
testResourcePostLoadDoIt
    "reset test resources"

    #(#'MetacelloAlternateResource' #'MetacelloAtomicConfigurationResource' #'MetacelloAtomicMonticelloResource' #'MetacelloConfigurationResource' #'MetacelloMonticelloResource' #'MetacelloScriptingResource' #'MetacelloIssue108Resource')
        do: [ :className | Smalltalk at: className ifPresent: [ :cls | cls reset ] ]