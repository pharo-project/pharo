writing
writeTraitDefinition: definition
    | properties compositionString |
    properties := Dictionary new.
    properties at: 'name' put: definition className.
    definition traitCompositionString
        ifNotNil: [ :property | 
            property ~= '{}'
                ifTrue: [ properties at: 'traitcomposition' put: property ] ].
    " handle the classTrait case "
    compositionString := self classTraitDefinitions at: definition className ifPresent: [:classTraitDefinition |
	classTraitDefinition classTraitCompositionString ].
    compositionString ifNil: [ compositionString := definition classTraitCompositionString ].
    compositionString~= '{}'
                ifTrue: [ properties at: 'classtraitcomposition' put: compositionString ] .
    properties at: 'category' put: definition category.
    properties at: 'commentStamp' put: definition commentStamp.
    STON put: properties asJsonOnStreamPretty: fileStream