handlers
handleLookupProjectSpecForLoad: exception
  "if overrideProjectSpec is nil, use currentVersion in image, ignoreImage is false"

  | requested override |
  requested := exception projectSpec.
  override := self useCurrentVersion
    ifTrue: [ 
      "don't do lookup in registry if we expect to use the #currentVersion calculation"
      nil ]
    ifFalse: [ 
      | registered |
      registered := self lookupProjectSpecFor: exception projectSpec.
      (registered compareEqual: requested)
        ifFalse: [ 
          "counts as override, only if they differ in some aspect"
          override := registered ] ].
  ^ exception
    resume:
      (MetacelloProjectSpecForLoad new
        projectSpec: requested;
        useDetermineVersionForLoad: self useCurrentVersion;
        overrideProjectSpec: override;
        yourself)