writing
writeTraitDefinition: definition
    stWriter writeClassDefinition: definition.
    definition hasComment 
        ifTrue: [stWriter writeClassComment: definition].