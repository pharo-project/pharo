validation
validateVersion: versionStringOrSymbol
    | issues |
    issues := (MetacelloMCVersionValidator validateProject: project version: versionStringOrSymbol)
        select: [ :issue | issue isError ].
    issues notEmpty
        ifTrue: [ 
            MetacelloValidationFailure
                issues: issues
                message: 'There are error issues with version ' , versionStringOrSymbol printString ]