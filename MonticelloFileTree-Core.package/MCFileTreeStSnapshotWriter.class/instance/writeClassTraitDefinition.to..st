writing
writeClassTraitDefinition: definition to: traitPath
    self
        writeInDirectoryName: traitPath
        fileName: definition className, '_classTrait'
        extension: '.st'
        visit: [ self writeClassTraitDefinition: definition ]