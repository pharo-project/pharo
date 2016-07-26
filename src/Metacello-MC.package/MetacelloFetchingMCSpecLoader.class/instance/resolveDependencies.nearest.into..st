private
resolveDependencies: aResolvedReference nearest: nearestReference into: aCollection
  | block retryCount coll notDone |
  self flag: 'Not used, but retained to avoid upgrade issues'.
  block := [ :dependency | 
  | reference |
  reference := MetacelloCachingGoferResolvedReference
    name: dependency versionInfo name
    repository: aResolvedReference repository.
  coll add: reference.
  Transcript
    cr;
    show:
        'Fetched dependency -> ' , reference name , ' --- '
            , reference repository repositoryDescription.
  self resolveDependencies: reference nearest: reference into: coll ].
  retryCount := 0.
  notDone := true.
  coll := OrderedCollection new.
  [ notDone and: [ retryCount < 3 ] ]
    whileTrue: [ 
      retryCount > 0
        ifTrue: [ 
          Transcript
            cr;
            show: '...RETRY' ].
      [ 
      "ensure that all resolved references have cached their version while wrapped by error handler"
      aCollection do: [ :each | each version ].
      nearestReference version dependencies do: block.
      notDone := false ]
        on: Error
        do: [ :ex | 
          retryCount := retryCount + 1.
          retryCount >= 3
            ifTrue: [ ex pass ].
          coll := OrderedCollection new ] ].
  aCollection addAll: coll.
  ^ aCollection