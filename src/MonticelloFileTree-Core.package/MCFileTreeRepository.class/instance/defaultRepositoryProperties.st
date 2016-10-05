accessing
defaultRepositoryProperties
  ^ Dictionary new
    at: 'packageExtension' put: self class defaultPackageExtension;
    at: 'propertyFileExtension' put: self class defaultPropertyFileExtension;
    yourself