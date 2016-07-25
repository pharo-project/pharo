api-configuration
updateVersionMethodForVersion: inputVersionStringOrSymbol updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock
    ^ self
        updateVersionMethodForVersion: inputVersionStringOrSymbol
        projectAttributes: project attributes
        updateProjects: updateProjects
        updatePackages: updatePackages
        versionSpecsDo: aBlock