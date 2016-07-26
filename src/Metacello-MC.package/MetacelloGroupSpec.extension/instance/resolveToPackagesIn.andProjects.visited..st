*metacello-mc
resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool visited: visited
  | packages |
  packages := Dictionary new.
  self
    resolveToPackagesIn: aVersionSpec
    andProjects: andProjectsBool
    into: packages
    visited: visited.
  ^ packages values asOrderedCollection