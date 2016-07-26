execution callback
executeCollection: aCollection do: projectSpecBlock
    self projectSpecsFromRepositoryArg
        do: [ :projectSpec | 
            (aCollection includes: (MetacelloScriptEngine baseNameOf: projectSpec className))
                ifTrue: [ projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ] ]