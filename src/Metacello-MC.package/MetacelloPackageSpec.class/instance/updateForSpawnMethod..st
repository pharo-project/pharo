development support
updateForSpawnMethod: sourceSpec
    "This means that this spec was used in a baseline and will be used in a version .... drop all information that isn't useful"

    | nm fl |
    nm := name.
    fl := file.
    fl == nil
        ifTrue: [ 
            "if only name has been set, then force the file to be non-nil, if any attribute besides file is set, then leave file nil"
            {answers.
            requires.
            includes.
            repositories.
            preLoadDoIt.
            postLoadDoIt} detect: [ :each | each ~~ nil ] ifNone: [ fl := name ] ].
    super updateForSpawnMethod: sourceSpec.
    file := repositories := goferPackage := preLoadDoIt := postLoadDoIt := nil.
    name := nm.
    file := fl.
    ^ file == nil