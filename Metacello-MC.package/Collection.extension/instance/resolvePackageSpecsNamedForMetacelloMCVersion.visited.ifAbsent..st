*metacello-mc
resolvePackageSpecsNamedForMetacelloMCVersion: aMetacelloMCVersion visited: visited ifAbsent: aBlock
    ^ aMetacelloMCVersion
        allPackagesForSpecs: (self collect: [ :ea | aMetacelloMCVersion packageNamed: ea ifAbsent: aBlock ])
        visited: visited