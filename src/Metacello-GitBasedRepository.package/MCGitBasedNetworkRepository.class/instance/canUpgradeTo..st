initialize-release
canUpgradeTo: anMCGitBasedRepository
  (anMCGitBasedRepository isKindOf: self class)
    ifFalse: [ ^ false ].
  ^ self projectPath = anMCGitBasedRepository projectPath
    and: [ self repoPath = anMCGitBasedRepository repoPath ]