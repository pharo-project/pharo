options
cacheRepository
    ^ (MetacelloMCProject new repositorySpec description: (self options at: #'cacheRepository' ifAbsent: [ ^ nil ]))
        createRepository