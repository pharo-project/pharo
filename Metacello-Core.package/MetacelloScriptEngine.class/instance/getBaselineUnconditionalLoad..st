project lookup
getBaselineUnconditionalLoad: unconditionalLoad
  | spec |
  spec := self projectSpec.
  Smalltalk
    at: spec className asSymbol
    ifPresent: [ :cl | 
      unconditionalLoad
        ifFalse: [ ^ cl ] ].
  spec := self lookupProjectSpecFor: spec.
  [ spec projectPackage load ]
    on: MetacelloIgnorePackageLoaded
    do: [ :ex | 
      "see https://github.com/dalehenrich/metacello-work/issues/244 ... uncoditionally load
        baseline"
      ex resume: true ].
  ^ Smalltalk at: spec className asSymbol