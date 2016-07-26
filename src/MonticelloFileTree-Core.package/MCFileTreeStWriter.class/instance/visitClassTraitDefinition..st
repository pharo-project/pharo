visiting
visitClassTraitDefinition: definition
    orderedTraitNames add: definition className, ' classTrait'.
    self
        writeInDirectoryName: definition className
        fileName: definition className, '_classTrait'
        extension: '.st'
        visit: [ self writeClassTraitDefinition: definition ]