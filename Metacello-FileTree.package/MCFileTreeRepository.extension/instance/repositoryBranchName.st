*metacello-filetree
repositoryBranchName
  "extract a branch name from the repository ... if possible"

  (MetacelloPlatform current gitBranchNameFor: self directory fullName)
    ifNotNil: [ :sha | ^ sha ].
  ^ super repositoryBranchName