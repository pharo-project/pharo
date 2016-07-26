handlers
handleLookupBaselineSpecForEnsureLoad: exception
  "if requested and registered don't compare equal, then ensure the new baseline is loaded"

  | requested registered |
  requested := exception projectSpec.
  registered := self lookupProjectSpecFor: exception projectSpec.
  ^ exception resume: (registered compareEqual: requested) not