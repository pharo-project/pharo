private
linearLoadPackageSpec: packageSpec gofer: gofer
  MetacelloPlatform current
    do: [ 
      | loadBlock goferLoad answers resolvedReference repo |
      resolvedReference := self resolvePackageSpec: packageSpec gofer: gofer.
      resolvedReference isNil
        ifTrue: [ 
          "Package version already loaded into image"
          ^ self ].
      loadBlock := [ 
      self preLoad: packageSpec.
      goferLoad := MetacelloGoferLoad on: MetacelloGofer new.
      goferLoad addResolved: resolvedReference.
      goferLoad execute.
      MetacelloPlatform current clearCurrentVersionCache.
      self postLoad: packageSpec ].
      (answers := packageSpec answers) notEmpty
        ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
        ifFalse: [ loadBlock value ].
      repo := resolvedReference repository.
      self hasRepositoryOverrides
        ifTrue: [ 
          repo := self loaderPolicy repositoryMap
            at: resolvedReference name
            ifAbsent: [ resolvedReference repository ].
          resolvedReference workingCopy repositoryGroup addRepository: repo ]
        ifFalse: [ 
          resolvedReference workingCopy repositoryGroup
            addRepository: resolvedReference repository ].
      Transcript
        cr;
        show:
            'Loaded -> ' , resolvedReference name , ' --- ' , repo repositoryDescription
                , ' --- ' , resolvedReference repository description ]
    displaying: 'Loading ' , packageSpec file