private
propertyFileExtension
  ^ self repositoryProperties
    at: 'propertyFileExtension'
    ifAbsent: [ self class defaultPropertyFileExtension ]