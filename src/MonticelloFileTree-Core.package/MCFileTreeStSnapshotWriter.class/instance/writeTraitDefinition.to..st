writing
writeTraitDefinition: definition to: traitPath
    self
        writeInDirectoryName: traitPath
        fileName: definition className
        extension: '.st'
        visit: [ self writeTraitDefinition: definition ]