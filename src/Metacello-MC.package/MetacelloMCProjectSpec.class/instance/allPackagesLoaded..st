testing
allPackagesLoaded: aLoader
    "answer true if all of the packages (excluding projects) are loaded"

    | vrsn pkgs |
    (vrsn := self versionOrNil) == nil
        ifTrue: [ ^ false ].
    pkgs := OrderedCollection new.
    (self loadListForVersion: vrsn)
        do: [ :nm | 
            vrsn packages
                do: [ :pkg | 
                    (pkg isPackageLoaded: aLoader)
                        ifFalse: [ ^ false ] ] ].
    ^ true