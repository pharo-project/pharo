visiting
visitTraitDefinition: definition
    orderedTraitNames add: definition className.
    self
        writeInDirectoryName: definition className
        fileName: definition className
        extension: '.st'
        visit: [ self writeTraitDefinition: definition ]