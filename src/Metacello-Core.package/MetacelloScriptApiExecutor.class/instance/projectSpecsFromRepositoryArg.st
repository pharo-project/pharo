execution callback
projectSpecsFromRepositoryArg
    | spec repo projectSpecs |
    repositoryArg ifNil: [ ^ #() ].
    spec := (projectSpecGenerator projectSpecCreationBlock value: 'xxx') first.
    repo := spec project createRepository: (spec project repositorySpec description: repositoryArg).
    projectSpecs := OrderedCollection new.
    ((Gofer new disablePackageCache
        repository: repo;
        allResolved) collect: [ :resolvedReference | resolvedReference packageName ]) asSet
        do: [ :packageName | 
            (projectSpecGenerator projectSpecCreationBlock value: (MetacelloScriptEngine baseNameOf: packageName))
                do: [ :projectSpec | 
                    projectSpec className = packageName
                        ifTrue: [ projectSpecs add: (self applyArgsToProjectSpec: projectSpec copy) ] ] ].
    ^ projectSpecs