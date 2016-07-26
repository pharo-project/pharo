*metacello-filetree
repositoryVersionString
  "extract a version string from the repository ... if possible"

  (MetacelloPlatform current gitCommitShaFor: self directory fullName)
    ifNotNil: [ :sha | ^ sha ].
  ^ super repositoryVersionString