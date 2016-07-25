writing
writeClassDefinition: definition
    stWriter writeClassDefinition: definition.
    (definition hasClassInstanceVariables or: [definition hasClassTraitComposition])
        ifTrue: [ stWriter writeMetaclassDefinition: definition ].
    definition hasComment
        ifTrue: [ stWriter writeClassComment: definition ]