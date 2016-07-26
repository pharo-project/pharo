*metacello-mc
resolveToPackagesIn: aVersionSpec into: packages visited: visited
  self
    resolveToPackagesIn: aVersionSpec
    andProjects: false
    into: packages
    visited: visited