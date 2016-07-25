utilities
addTraitDefinitionFrom: traitPropertiesDict comment: traitComment
    definitions
        add:
            (MCTraitDefinition
                name: (traitPropertiesDict at: 'name')
                traitComposition: (traitPropertiesDict at: 'traitcomposition' ifAbsent: [ '{}' ])
                category: (traitPropertiesDict at: 'category' ifAbsent: [ self packageNameFromPackageDirectory ])
                comment: traitComment
                commentStamp: (traitPropertiesDict at: 'commentStamp' ifAbsent: [ '' ])).
    traitPropertiesDict at: 'classtraitcomposition' ifPresent: [:classTraitComposition |
        definitions
             add:
                (MCClassTraitDefinition
                    baseTraitName: (traitPropertiesDict at: 'name')
                    classTraitComposition: classTraitComposition)].