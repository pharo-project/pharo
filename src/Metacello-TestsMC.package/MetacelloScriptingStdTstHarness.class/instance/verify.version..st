utilities
verify: packageName version: fileName
    | externalCoreWorkingCopy x |
    externalCoreWorkingCopy := MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ].
    self assert: (x := externalCoreWorkingCopy ancestors first name) = fileName