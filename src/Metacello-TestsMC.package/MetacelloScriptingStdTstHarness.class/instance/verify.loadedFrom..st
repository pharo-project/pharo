utilities
verify: packageName loadedFrom: repositoryDescription
    | externalCoreWorkingCopy |
    externalCoreWorkingCopy := MCWorkingCopy allManagers detect: [ :wc | wc packageName = packageName ].
    self
        assert:
            (externalCoreWorkingCopy repositoryGroup repositories
                includes: (MetacelloMCProject new repositorySpec description: repositoryDescription) createRepository)