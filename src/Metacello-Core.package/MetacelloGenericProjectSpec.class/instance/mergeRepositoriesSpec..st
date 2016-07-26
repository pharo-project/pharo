merging
mergeRepositoriesSpec: anotherRepositories
  self
    repositories:
      (self getRepositories == nil
        ifTrue: [ anotherRepositories ]
        ifFalse: [ self repositories mergeSpec: anotherRepositories ])