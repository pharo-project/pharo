private
ensureGoferVersion: goferVersion repositoryUrl: repositoryUrl
    "load the p=file goferVersion if Gofer isn't loaded or an earlier version of Gofer is currently loaded"

    | goferVersionNumber wc pName |
    (Smalltalk at: #'Gofer' ifAbsent: [  ]) == nil
        ifTrue: [ ^ self bootstrapPackage: goferVersion from: repositoryUrl ].
    goferVersionNumber := (goferVersion copyAfterLast: $.) asNumber.
    wc := [ ((Smalltalk at: #'GoferPackageReference') name: 'Gofer') workingCopy ]
        on: Error
        do: [ :ex | ex return: ((Smalltalk at: #'GoferPackageReference') name: 'Gofer-Core') workingCopy ].
    pName := wc ancestry ancestors first name.
    (pName copyAfterLast: $.) asNumber <= goferVersionNumber
        ifTrue: [ self bootstrapPackage: goferVersion from: repositoryUrl ]