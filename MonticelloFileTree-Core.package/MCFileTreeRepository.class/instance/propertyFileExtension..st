private
propertyFileExtension: propertyFileExtension
  self class validatePropertyFileExtension: propertyFileExtension.
  self repositoryProperties
    at: 'propertyFileExtension'
    put: propertyFileExtension.
  self writeRepositoryProperties