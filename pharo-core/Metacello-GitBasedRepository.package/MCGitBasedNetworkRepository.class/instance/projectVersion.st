accessing
projectVersion
    (projectVersion == nil or: [ projectVersion isEmpty ])
        ifTrue: [ projectVersion := 'master' ].
    ^ projectVersion