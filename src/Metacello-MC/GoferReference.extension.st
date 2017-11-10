Extension { #name : #GoferReference }

{ #category : #'*metacello-mc' }
GoferReference >> matchesMetacelloGoferPackage: aMetacelloGoferPackage [
  | pFilename refFilename char |
  ((pFilename := aMetacelloGoferPackage packageFilename) == nil
    or: [ aMetacelloGoferPackage name = aMetacelloGoferPackage packageFilename ])
    ifTrue: [ ^ super matches: self ].
  self name = pFilename
    ifTrue: [ ^ true ].
  (self name beginsWith: pFilename)
    ifFalse: [ ^ false ].
  refFilename := self metacelloPackageNameWithBranch at: 2.
  refFilename = pFilename
    ifTrue: [ ^ true ].
  pFilename size < refFilename size
    ifTrue: [ 
      (refFilename beginsWith: pFilename)
        ifFalse: [ ^ false ].
      (char := pFilename at: pFilename size) ~= $-
        ifTrue: [ char := refFilename at: pFilename size + 1 ] ]
    ifFalse: [ 
      (pFilename beginsWith: refFilename)
        ifFalse: [ ^ false ].
      (char := refFilename at: refFilename size) ~= $-
        ifTrue: [ char := pFilename at: refFilename size + 1 ] ].
  ^ char = $. or: [ char = $- ]
]
