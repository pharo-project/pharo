private
packageExtension
  ^ self repositoryProperties
    at: 'packageExtension'
    ifAbsent: [ self class defaultPackageExtension ]