scripting
hasNoLoadConflicts: aMetacelloRepositoriesSpec
  | repositorySpecs anotherRepositorySpecs |
  repositorySpecs := self map values
    sort: [ :a :b | a description <= b description ].
  anotherRepositorySpecs := aMetacelloRepositoriesSpec map values
    sort: [ :a :b | a description <= b description ].
  repositorySpecs size ~= anotherRepositorySpecs size
    ifTrue: [ ^ false ].
  1 to: repositorySpecs size do: [ :index | 
    | repoSpec anotherRepoSpec |
    repoSpec := repositorySpecs at: index.
    anotherRepoSpec := anotherRepositorySpecs at: index.
    (repoSpec hasNoLoadConflicts: anotherRepoSpec)
      ifFalse: [ ^ false ] ].
  ^ true