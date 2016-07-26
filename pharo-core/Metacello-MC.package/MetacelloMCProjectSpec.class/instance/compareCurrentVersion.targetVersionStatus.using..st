testing
compareCurrentVersion: anOperator targetVersionStatus: targetVersionStatus using: anMCLoader
    ^ (MetacelloLookupProjectSpecForLoad new
        projectSpec: self;
        yourself) signal
        performCurrentVersionTestAgainst: self versionOrNil
        operator: anOperator
        targetVersionStatus: targetVersionStatus
        using: anMCLoader