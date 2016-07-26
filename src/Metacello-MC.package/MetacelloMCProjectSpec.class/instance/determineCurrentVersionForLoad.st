loading
determineCurrentVersionForLoad
    "don't use self currentVersion, because we are interested in the currentVersion of the project as loaded in image, not the current version relative to our load list"

    | prjct version currentVersion packageAndProjectNames cvs |
    self projectClass == nil
        ifTrue: [ ^ nil ].
    (version := self versionOrNil) == nil
        ifTrue: [ ^ nil ].
    version blessing == #'baseline'
        ifTrue: [ ^ version ].
    self loader ignoreImage
        ifTrue: [ ^ version ].
    prjct := self projectClass new project.
    prjct loader: self loader.
    (currentVersion := prjct currentVersion) == nil
        ifTrue: [ ^ nil ].
    (cvs := currentVersion versionStatus) == #'somethingLoaded'
        ifTrue: [ ^ nil ].
    (#(#'allLoadedToSpec' #'loadedToSpec' #'loadedMatchConstraints') includes: (cvs := currentVersion versionStatus))
        ifTrue: [ 
            (currentVersion perform: self operator with: version)
                ifTrue: [ 
                    "load currentVersion"
                    ^ currentVersion ].	"load version"
            ^ nil ].
    version = currentVersion
        ifTrue: [ ^ currentVersion ].	"I don't believe that it is possible to reach this point in the method, so I will be interested if I run across a case that produces this error"
    (MetacelloProjectSpecLoadConflict projectSpec: self)
        signal:
            'Project load conflict for' , prjct label printString , ' between current version ' , currentVersion printString , '('
                , cvs asString , ') and specified version ' , version printString
                , '. Press resume to continue with load anyway'.
    ^ nil