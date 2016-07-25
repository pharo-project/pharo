writing
writeExtensionClassDefinition: definition to: classPath
  self
    writeInDirectoryName: classPath
    fileName: 'properties'
    extension: self propertyFileExtension
    visit: [ self writeExtensionClassDefinition: definition ]