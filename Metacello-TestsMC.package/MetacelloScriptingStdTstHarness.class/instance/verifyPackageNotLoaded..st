utilities
verifyPackageNotLoaded: packageName
    self assert: (MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ] ifNone: [  ]) == nil