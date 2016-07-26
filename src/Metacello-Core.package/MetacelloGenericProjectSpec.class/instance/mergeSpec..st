merging
mergeSpec: anotherSpec
  | newSpec map anotherRepositories |
  newSpec := super mergeSpec: anotherSpec.
  map := anotherSpec mergeMap.
  (anotherRepositories := map at: #'repositories') ~~ nil
    ifTrue: [ newSpec mergeRepositoriesSpec: anotherRepositories ].
  ^ newSpec