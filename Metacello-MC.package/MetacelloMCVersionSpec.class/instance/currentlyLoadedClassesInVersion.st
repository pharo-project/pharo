querying
currentlyLoadedClassesInVersion
  | classes |
  classes := Set new.
  self
    projectDo: [ :ignored |  ]
    packageDo: [ :packageSpec | 
      | wc |
      wc := [ packageSpec workingCopy ]
        on: Error
        do: [ :ex | ex return: nil ].
      wc ~~ nil
        ifTrue: [ classes addAll: (MetacelloPlatform current packageInfoFor: wc) classes ] ]
    groupDo: [ :ignored |  ].
  ^ classes