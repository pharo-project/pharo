execution
applyArgsToProjectSpec: aProjectSpec
    classNameArg ifNotNil: [ aProjectSpec className: classNameArg ].
    versionArg ifNotNil: [ aProjectSpec versionString: versionArg ].
    repositoryArg ifNotNil: [ aProjectSpec repository: repositoryArg ].
    ^ aProjectSpec