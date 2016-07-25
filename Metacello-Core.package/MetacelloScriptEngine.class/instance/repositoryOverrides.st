options
repositoryOverrides
    ^ (self options at: #'repositoryOverrides' ifAbsent: [ ^ nil ])
        collect: [ :description | (MetacelloMCProject new repositorySpec description: description) createRepository ]