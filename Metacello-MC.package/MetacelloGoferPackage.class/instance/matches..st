private
matches: aLoadableReference
  | pFilename |
  ((pFilename := self packageFilename) == nil
    or: [ self name = self packageFilename ])
    ifTrue: [ ^ super matches: aLoadableReference ].
  aLoadableReference name = pFilename
    ifTrue: [ ^ true ].
  (aLoadableReference name beginsWith: pFilename)
    ifFalse: [ ^ false ].
  ^ aLoadableReference matchesMetacelloGoferPackage: self