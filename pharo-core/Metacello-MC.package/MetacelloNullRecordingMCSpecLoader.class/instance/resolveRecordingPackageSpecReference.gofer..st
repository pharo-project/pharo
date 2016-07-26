private
resolveRecordingPackageSpecReference: packageSpec gofer: gofer
  | externalPackageReference |
  externalPackageReference := packageSpec file == nil
    ifTrue: [ GoferPackageReference name: packageSpec name ]
    ifFalse: [ GoferResolvedReference name: packageSpec file repository: nil ].
  packageSpec repositorySpecs isEmpty
    ifTrue: [ 
      self repositoryMap
        at: externalPackageReference packageName
        put:
          (gofer repositories
            reject: [ :repo | repo = MetacelloPlatform current defaultPackageCache ]) ]
    ifFalse: [ 
      self repositoryMap
        at: externalPackageReference packageName
        put:
          (packageSpec repositorySpecs collect: [ :repoSpec | repoSpec createRepository ]) ].
  ^ externalPackageReference