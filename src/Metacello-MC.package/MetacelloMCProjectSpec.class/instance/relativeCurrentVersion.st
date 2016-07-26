querying
relativeCurrentVersion
    "currentVersion calculated relative to the loadList"

    | vrsn expanded loadList |
    (vrsn := self versionOrNil) == nil
        ifTrue: [ ^ nil ].
    expanded := [ vrsn expandToLoadableSpecNames: (loadList := self loadListForVersion: vrsn) ]
        on: Error
        do: [ :ex | 
            vrsn blessing == #'development'
                ifTrue: [ 
                    self ensureLoadUsing: self loader.
                    vrsn := self versionOrNil.
                    ex return: (vrsn expandToLoadableSpecNames: loadList) ].
            ex pass ].
    ^ self projectClassProject currentVersionAgainst: expanded