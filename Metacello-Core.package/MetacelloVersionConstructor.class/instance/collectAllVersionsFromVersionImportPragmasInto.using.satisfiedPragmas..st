private
collectAllVersionsFromVersionImportPragmasInto: versionMap using: executionBlock satisfiedPragmas: pragmaDict
    | defined done completed count |
    done := false.
    completed := IdentitySet new.
    count := 0.
    [ 
    count := count + 1.
    count > 10000
        ifTrue: [ self error: 'Apparent loop in import expansion' ].
    done ]
        whileFalse: [ 
            done := true.
            pragmaDict
                keysAndValuesDo: [ :versionString :pragmaColl | 
                    | versionSpec |
                    versionSpec := nil.
                    defined := false.
                    [ 
                    pragmaColl
                        do: [ :pragma | 
                            (completed includes: pragma)
                                ifFalse: [ 
                                    | imports |
                                    done := false.
                                    imports := pragma argumentAt: 2.
                                    imports
                                        detect: [ :importedVersion | (versionMap includesKey: importedVersion) not ]
                                        ifNone: [ 
                                            imports
                                                do: [ :importedVersion | 
                                                    | version |
                                                    (version := versionMap at: importedVersion ifAbsent: [  ]) ~~ nil
                                                        ifTrue: [ 
                                                            defined := true.
                                                            completed add: pragma.
                                                            versionSpec == nil
                                                                ifTrue: [ versionSpec := version spec copy ]
                                                                ifFalse: [ versionSpec := versionSpec mergeSpec: version spec copy ].
                                                            versionSpec versionString: versionString.
                                                            executionBlock value: versionSpec value: pragma ] ] ] ] ] ]
                        on: Error
                        do: [ :ex | 
                            (MetacelloErrorInProjectConstructionNotification versionString: versionSpec versionString exception: ex)
                                ifTrue: [ ^ ex pass ]
                                ifFalse: [ 
                                    self errorMap at: versionSpec versionString put: ex.
                                 
                                    defined := false ] ].
                    defined
                        ifTrue: [ 
                            | version importedVersions |
                            importedVersions := OrderedCollection new.
                            version := versionSpec createVersion.
                            pragmaColl do: [ :pragma | importedVersions addAll: (pragma argumentAt: 2) ].
                            version importedVersions: importedVersions.
                            self validateVersionString: versionString againstSpec: versionSpec.
                            versionMap at: versionSpec versionString put: version ].
                    self reset ] ]