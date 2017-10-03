actions
linearLoadPackageSpec: packageSpec gofer: gofer
  MetacelloPlatform current
    do: [ 
      | references nearestReference cachedReference externalReference mcVersion loadedVersionInfos |
      cachedReference := nil.
      packageSpec
        searchCacheRepositoryForPackage: [ 
          "check to see if mcz file is already in cacheRepository"
          cachedReference := self
            resolvePackageSpec: packageSpec
            cachedGofer: self loaderPolicy cacheGofer.
          (cachedReference ~~ nil and: [ packageSpec getFile ~~ nil ])
            ifTrue: [ 
              cachedReference name = packageSpec file
                ifTrue: [ 
                  "exact match between packageSpec file and cache"
                  ^ self
                    scheduleFetchFor: packageSpec
                    cachedReference: cachedReference ] ] ].
      references := self
        retryingResolvePackageSpecReferences: packageSpec
        gofer: gofer.	"look up mcz file"
      nearestReference := references last asMetacelloCachingResolvedReference.
      (cachedReference ~~ nil
        and: [ cachedReference name = nearestReference name ])
        ifTrue: [ 
          "latest reference in repository matches cachedReference ... "
          ^ self
            scheduleFetchFor: packageSpec
            nearestReference: nearestReference ].
      (self ignoreImage not
        and: [ (loadedVersionInfos := self ancestorsFor: packageSpec) ~~ nil ])
        ifTrue: [ 
          "If the mcz is already loaded into the image, no need to copy"
          loadedVersionInfos
            do: [ :info | 
              info name = nearestReference name
                ifTrue: [ 
                  | spc |
                  spc := packageSpec copy.
                  spc file: info name.
                  (MetacelloIgnorePackageLoaded signal: spc)
                    ifFalse: [ ^ self ] ] ] ].
      externalReference := (references
        select: [ :ref | ref name = nearestReference name ]) first
        asMetacelloCachingResolvedReference.
      self repositoryMap
        at: externalReference name
        put: externalReference repository.
      (self
        resolveDependencies: externalReference
        nearest: nearestReference
        into: (OrderedCollection with: nearestReference))
        do: [ :reference | 
          | pSpec l |
          mcVersion := reference version.
          (l := (GoferVersionReference name: reference name)
            resolveAllWith: self loaderPolicy cacheGofer) isEmpty
            ifTrue: [ 
              self cacheRepository storeVersion: mcVersion.
              reference == nearestReference
                ifTrue: [ pSpec := packageSpec ]
                ifFalse: [ 
                  pSpec := packageSpec project packageSpec.
                  pSpec name: mcVersion package name ].
              self loadData
                addVersion: mcVersion
                versionInfo: mcVersion info
                resolvedReference: reference
                packageSpec: pSpec ] ].
      self scheduleFetchFor: packageSpec externalReference: externalReference ]
    displaying: 'Fetching ' , packageSpec file