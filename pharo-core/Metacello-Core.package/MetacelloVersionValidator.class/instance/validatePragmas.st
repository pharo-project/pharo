private
validatePragmas
    "Ensure that versions are Strings and symbolic versions are symbols.
	 Ensure that imported versions are defined"

    | versionPragmas versionImportPragmas symbolicVersionPragmas versionStrings |
    versionPragmas := self extractVersionPragmas.
    versionImportPragmas := self extractVersionImportPragmas.
    symbolicVersionPragmas := self extractSymbolicVersionPragmas.
    versionStrings := Set new.
    versionPragmas keys , versionImportPragmas keys , symbolicVersionPragmas keys
        do: [ :versionString | 
            (versionStrings includes: versionString)
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ]
                ifFalse: [ versionStrings add: versionString ] ].
    versionPragmas
        keysAndValuesDo: [ :versionString :pragmaList | 
            versionString isString
                ifFalse: [ 
                    self
                        recordValidationError: 'version ' , versionString printString , ' is not a String.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'invalidVersionString' ].
            pragmaList size > 1
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ] ].
    symbolicVersionPragmas
        keysAndValuesDo: [ :versionString :pragmaList | 
            versionString isSymbol
                ifFalse: [ 
                    self
                        recordValidationError: 'symbolic version ' , versionString printString , ' is not a Symbol.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'invalidVersionString' ].
            pragmaList size > 1
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ] ].
    versionImportPragmas
        keysAndValuesDo: [ :versionString :pragmaList | 
            versionString isString
                ifFalse: [ 
                    self
                        recordValidationError: 'version ' , versionString printString , ' is not a String.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'invalidVersionString' ].
            pragmaList size > 1
                ifTrue: [ 
                    self
                        recordValidationCriticalWarning:
                            'version ' , versionString printString , ' is defined multiple times.'
                        versionString: versionString
                        callSite: #'validatePragmas'
                        reasonCode: #'duplicateVersionDefinitions' ].
            pragmaList
                do: [ :versionImportPragma | 
                    (versionImportPragma arguments at: 2)
                        do: [ :importedVersionString | 
                            (versionStrings includes: importedVersionString)
                                ifFalse: [ 
                                    self
                                        recordValidationError:
                                            'version ' , importedVersionString printString , ' referenced in import list of version ' , versionString printString
                                                , ' has not been defined.'
                                        versionString: versionString
                                        callSite: #'validatePragmas'
                                        reasonCode: #'missingVersionImport' ] ] ] ]