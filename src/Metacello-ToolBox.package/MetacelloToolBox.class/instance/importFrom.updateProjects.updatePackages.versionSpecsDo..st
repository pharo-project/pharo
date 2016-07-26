api-configuration
importFrom: inputVersionStringOrSymbol updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock
    ^ self
        importFrom: inputVersionStringOrSymbol
        projectAttributes: project attributes
        inactiveAttributes: #()
        updateProjects: updateProjects
        updatePackages: updatePackages
        versionSpecsDo: aBlock