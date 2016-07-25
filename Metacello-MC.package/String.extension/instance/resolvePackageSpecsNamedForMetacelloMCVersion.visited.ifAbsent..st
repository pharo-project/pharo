*metacello-mc
resolvePackageSpecsNamedForMetacelloMCVersion: aMetacelloMCVersion visited: visited ifAbsent: aBlock
    ^ aMetacelloMCVersion
        allPackagesForSpecs: {(aMetacelloMCVersion packageNamed: self ifAbsent: aBlock)}
        visited: visited