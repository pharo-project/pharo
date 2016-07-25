querying
repositoryDescriptions
    ^ self repositorySpecs collect: [ :repoSpec | repoSpec description ]