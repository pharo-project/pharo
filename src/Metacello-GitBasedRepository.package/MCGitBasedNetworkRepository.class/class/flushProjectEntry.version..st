utility
flushProjectEntry: projectPath version: versionString
    self downloadCache removeKey: (self downloadCacheKey: projectPath version: versionString) ifAbsent: [  ]