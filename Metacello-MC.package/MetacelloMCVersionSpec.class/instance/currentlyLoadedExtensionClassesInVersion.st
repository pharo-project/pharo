querying
currentlyLoadedExtensionClassesInVersion
  | classes |
  classes := Dictionary new.
  self
    projectDo: [ :ignored |  ]
    packageDo: [ :packageSpec | 
      | wc |
      wc := [ packageSpec workingCopy ]
        on: Error
        do: [ :ex | ex return: nil ].
      wc ~~ nil
        ifTrue: [ 
          | packageInfo |
          packageInfo := MetacelloPlatform current packageInfoFor: wc.
          packageInfo extensionClasses
            do: [ :cl | classes at: cl put: (packageInfo extensionCategoriesForClass: cl) ] ] ]
    groupDo: [ :ignored |  ].
  ^ classes