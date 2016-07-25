utilities
addClassDefinitionFrom: classPropertiesDict comment: classComment
  | categoryName className |
  className := classPropertiesDict at: 'name'.
  categoryName := classPropertiesDict
    at: 'category'
    ifAbsent: [ self packageNameFromPackageDirectory ].
  self validateClassCategory: categoryName for: className.
  definitions
    add:
      (MCClassDefinition
        name: className
        superclassName: (classPropertiesDict at: 'super')
        traitComposition: (classPropertiesDict at: 'traitcomposition' ifAbsent: [ '{}' ])
        classTraitComposition: (classPropertiesDict at: 'classtraitcomposition' ifAbsent: [ '{}' ])
        category: categoryName
        instVarNames: (classPropertiesDict at: 'instvars' ifAbsent: [ #() ])
        classVarNames: (classPropertiesDict at: 'classvars' ifAbsent: [ #() ])
        poolDictionaryNames: (classPropertiesDict at: 'pools' ifAbsent: [ #() ])
        classInstVarNames:
          (classPropertiesDict at: 'classinstvars' ifAbsent: [ #() ])
        type: (classPropertiesDict at: 'type' ifAbsent: [ 'normal' ]) asSymbol
        comment: classComment
        commentStamp: (classPropertiesDict at: 'commentStamp' ifAbsent: [ '' ]))

