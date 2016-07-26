execution callback
executeString: aString do: projectSpecBlock
  singleRoot ifNil: [ self singleRoot: true ].
  (projectSpecGenerator projectSpecCreationBlock value: aString)
    do: [ :projectSpec | 
      projectSpec
        ifNotNil: [ projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ] ]