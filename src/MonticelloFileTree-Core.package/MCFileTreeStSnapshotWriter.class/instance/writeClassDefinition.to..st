writing
writeClassDefinition: definition to: classPath
    self
        writeInDirectoryName: classPath
        fileName: definition className
        extension: '.st'
        visit: [ self writeClassDefinition: definition ]