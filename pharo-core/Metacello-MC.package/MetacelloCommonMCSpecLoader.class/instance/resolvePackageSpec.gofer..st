private
resolvePackageSpec: packageSpec gofer: gofer
  | references resolvedReference mcVersion loadedVersionInfos |
  references := self
    retryingResolvePackageSpecReferences: packageSpec
    gofer: gofer.
  resolvedReference := references last asMetacelloCachingResolvedReference.
  mcVersion := resolvedReference version.
  (loadedVersionInfos := self ancestorsFor: packageSpec) ~~ nil
    ifTrue: [ 
      loadedVersionInfos
        do: [ :info | 
          info name = mcVersion info name
            ifTrue: [ 
              | spc |
              "package already loaded, don't load again"
              spc := packageSpec copy.
              spc file: info name.
              (MetacelloIgnorePackageLoaded signal: spc)
                ifFalse: [ ^ nil ] ] ] ].
  ^ resolvedReference