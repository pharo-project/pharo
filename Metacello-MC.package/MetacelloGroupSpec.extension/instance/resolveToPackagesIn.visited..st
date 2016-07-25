*metacello-mc
resolveToPackagesIn: aVersionSpec visited: visited
  | packages |
  packages := Dictionary new.
  self
    resolveToPackagesIn: aVersionSpec
    andProjects: false
    into: packages
    visited: visited.
  ^ packages values asOrderedCollection