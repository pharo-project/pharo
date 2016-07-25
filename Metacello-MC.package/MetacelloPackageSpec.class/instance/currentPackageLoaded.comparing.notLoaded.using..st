testing
currentPackageLoaded: loadedBlock comparing: comarisonOperator notLoaded: notLoadedBlock using: anMCLoader
  "Use currentVersionInfoFor: because it involves the loader and returns versionInfo for a planned load 
	 (atomic loaders) or currently loaded package"

  | wcName vis |
  vis := anMCLoader ancestorsFor: self.
  vis notNil
    ifTrue: [ 
      | fileRef wcRef |
      self getFile == nil
        ifTrue: [ ^ loadedBlock value: false ].
      vis
        do: [ :vi | 
          wcName := vi name.
          fileRef := GoferVersionReference name: self file.
          fileRef versionNumber = 0
            ifTrue: [ 
              "a shame that GoferVersionReference doesn't have better method for recognizing a missing verion number"
              "fix for: https://github.com/dalehenrich/metacello-work/issues/185"
              ^ loadedBlock value: false ].
          wcRef := GoferVersionReference name: wcName.
          (wcRef compare: fileRef using: comarisonOperator)
            ifTrue: [ ^ loadedBlock value: true ] ].
      ^ loadedBlock value: false ].
  ^ notLoadedBlock value: true