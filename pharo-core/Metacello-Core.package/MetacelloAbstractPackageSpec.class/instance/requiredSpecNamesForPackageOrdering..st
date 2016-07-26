private
requiredSpecNamesForPackageOrdering: aVersionSpec
  ^ (self requiredSpecsForPackageOrdering: aVersionSpec)
    collect: [ :spec | spec name ]