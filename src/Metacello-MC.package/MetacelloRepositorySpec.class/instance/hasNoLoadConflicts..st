mc support
hasNoLoadConflicts: aMetacelloRepositorySpec
  self description = aMetacelloRepositorySpec description
    ifTrue: [ ^ true ].
  self type = aMetacelloRepositorySpec type
    ifFalse: [ 
      "if the types are different then we don't need to create the repoitory to 
       know that there will be conflicts"
      ^ false ].
  (#('github' 'gitorious' 'bitbucket') includes: self type)
    ifTrue: [ 
      ^ self createRepository
        hasNoLoadConflicts: aMetacelloRepositorySpec createRepository ].
  ^ false