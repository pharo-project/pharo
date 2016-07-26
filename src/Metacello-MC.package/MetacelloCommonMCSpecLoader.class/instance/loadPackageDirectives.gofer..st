actions
loadPackageDirectives: pkgLoads gofer: aGofer
  MetacelloPlatform current
    do: [ 
      | goferLoad loadBlock answers |
      goferLoad := MetacelloGoferLoad on: aGofer.
      answers := OrderedCollection new.
      pkgLoads
        do: [ :packageLoadDirective | 
          | resolvedReference |
          aGofer disablePackageCache.	"for good luck:)"
          (resolvedReference := self
            resolvePackageSpec: packageLoadDirective spec
            gofer: aGofer) ~~ nil
            ifTrue: [ 
              goferLoad addResolved: resolvedReference.
              answers addAll: packageLoadDirective spec answers.
              packageLoadDirective resolvedReference: resolvedReference ] ].
      Transcript
        cr;
        show: 'Starting atomic load'.
      loadBlock := [ 
      "pkgLoads do: [:packageLoadDirective |  mcLoader preLoad: packageLoadDirective spec ]."
      goferLoad execute.
      pkgLoads
        do: [ :packageLoadDirective | 
          packageLoadDirective resolvedReference == nil
            ifTrue: [ 
              Transcript
                cr;
                tab;
                show: 'Already Loaded -> ' , packageLoadDirective file ]
            ifFalse: [ 
              Transcript
                cr;
                tab;
                show:
                    'Loaded -> ' , packageLoadDirective file , ' --- '
                        , packageLoadDirective repository repositoryDescription
                        , ' --- '
                        ,
                          packageLoadDirective resolvedReference repository description.
              packageLoadDirective resolvedReference workingCopy repositoryGroup
                addRepository: packageLoadDirective repository ] ].
      MetacelloPlatform current clearCurrentVersionCache	"pkgLoads do: [:packageLoadDirective | 
					packageLoadDirective resolvedReference ~~ nil
						ifTrue: [mcLoader postLoad: packageLoadDirective spec ]]" ].
      answers notEmpty
        ifTrue: [ loadBlock valueSupplyingMetacelloAnswers: answers ]
        ifFalse: [ loadBlock value ].
      Transcript
        cr;
        show: 'Finished atomic load' ]
    displaying: 'Atomic Load...'