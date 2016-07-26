mc support
hasNoLoadConflicts: aMetacelloRepositorySpec
  self description = aMetacelloRepositorySpec description
    ifTrue: [ ^ true ].
  (#('github' 'gitorious' 'bitbucket') includes: self type)
    ifTrue: [ 
      ^ self createRepository
        hasNoLoadConflicts: aMetacelloRepositorySpec createRepository ].
  ^ false