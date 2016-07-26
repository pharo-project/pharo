execution callback
executeCollection: aCollection do: projectSpecBlock
    aCollection
        do: [ :projectName | 
            ((projectSpecGenerator projectSpecLookupBlock value: projectName) select: self projectSpecSelectBlock)
                do: [ :projectSpec | projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ] ]