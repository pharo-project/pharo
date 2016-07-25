*Versionner-Commit
fixedImportFrom: inputVersionStringOrSymbol updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock
    ^ self
        fixedImportFrom: inputVersionStringOrSymbol
        projectAttributes: project attributes
        inactiveAttributes: #()
        updateProjects: updateProjects
        updatePackages: updatePackages
        versionSpecsDo: aBlock