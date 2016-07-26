private
resolveToAllPackagesIn: aVersionSpec visited: visited
    | vrsn |
    visited
        pushProject: [ 
            visited
                visit: self
                doing: [ :spec | 
                    spec ensureProjectLoaded.
                    vrsn := spec version.
                    ^ vrsn
                        allPackagesForSpecNamed: (self loadListForVersion: vrsn)
                        ifAbsent: [ self error: 'invalid loads: spec' ] ] ].
    ^ #()