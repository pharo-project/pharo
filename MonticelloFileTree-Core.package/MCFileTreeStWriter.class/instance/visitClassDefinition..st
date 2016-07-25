visiting
visitClassDefinition: definition
    orderedClassNames add: definition className.
    self
        writeInDirectoryName: definition className
        fileName: definition className
        extension: '.st'
        visit: [ self writeClassDefinition: definition ]