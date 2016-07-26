actions
loadPackageDirective: aPackageLoadDirective gofer: aGofer
  | packageSpec |
  packageSpec := aPackageLoadDirective spec.
  MetacelloPlatform current
    do: [ 
      | loadBlock goferLoad answers resolvedReference |
      aGofer disablePackageCache.	"for good luck:)"
      resolvedReference := self resolvePackageSpec: packageSpec gofer: aGofer.
      resolvedReference isNil
        ifTrue: [ 
          "Package version already loaded into image"
          ^ self ].
      loadBlock := [ 
      "mcLoader preLoad: packageSpec."
      goferLoad := MetacelloGoferLoad on: aGofer.
      goferLoad addResolved: resolvedReference.
      goferLoad execute.
      MetacelloPlatform current clearCurrentVersionCache	"mcLoader postLoad: packageSpec" ].
      (answers := packageSpec answers) notEmpty
        ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
        ifFalse: [ loadBlock value ].
      resolvedReference workingCopy repositoryGroup
        addRepository: aPackageLoadDirective repository.
      Transcript
        cr;
        show:
            'Loaded -> ' , resolvedReference name , ' --- '
                , aPackageLoadDirective repository repositoryDescription
                , ' --- ' , resolvedReference repository description ]
    displaying: 'Loading ' , packageSpec file