execution callback
executeBlock: selectBlock do: projectSpecBlock
    (self projectSpecsFromRepositoryArg select: selectBlock)
        do: [ :projectSpec | projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ]