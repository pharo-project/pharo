printing
configMethodOn: aStream last: last indent: indent
    | spec hasPackageSpecs |
    hasPackageSpecs := false.
    self packagesSpec list
        do: [ :member | 
            member spec
                projectDo: [ :proj | 
                    member spec name ~~ nil
                        ifTrue: [ hasPackageSpecs := true ] ]
                packageDo: [ :package | 
                    member spec name ~~ nil
                        ifTrue: [ hasPackageSpecs := true ] ]
                groupDo: [ :group | 
                    member spec name ~~ nil
                        ifTrue: [ hasPackageSpecs := true ] ] ].
    self configMethodBasicOn: aStream last: hasPackageSpecs not indent: indent.
    self configPackagesSpecMethodOn: aStream indent: indent.
    last
        ifFalse: [ aStream cr ]