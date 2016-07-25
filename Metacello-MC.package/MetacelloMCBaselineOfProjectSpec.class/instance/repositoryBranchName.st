querying
repositoryBranchName
  "extract a branch name from the repository ... if possible"

  "must parallel implementation of MetacelloMCBaselineProject>>setBaselineRepositoryDescription: we want the same repoSpec"

  | spec repo |
  self repositorySpecs do: [ :repoSpec | spec := repoSpec ].
  repo := spec createRepository.
  ^ repo repositoryBranchName