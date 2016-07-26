running
tearDownPackages
    | aGofer |
    aGofer := Gofer new.
    self tearDownPackageList
        do: [ :packageName | 
            (self hasPackage: packageName)
                ifTrue: [ aGofer package: packageName ] ].
    aGofer references notEmpty
        ifTrue: [ aGofer metacelloUnload ]