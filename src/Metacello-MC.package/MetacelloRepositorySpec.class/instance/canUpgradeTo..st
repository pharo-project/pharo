mc support
canUpgradeTo: aMetacelloRepositorySpec
  self description = aMetacelloRepositorySpec description
    ifTrue: [ ^ true ].
  (#('github' 'gitorious' 'bitbucket') includes: self type)
    ifTrue: [ ^ self createRepository canUpgradeTo: aMetacelloRepositorySpec createRepository ].
  ^ false