scripting
hasNoLoadConflicts: aMetacelloProjectSpec
  "same as compareEqual:, except if versionString and/or repositories are not 
	specified for either one, then there are not conflicts"

  ^ self className = aMetacelloProjectSpec className
    and: [ 
      (self compareVersionsEqual: aMetacelloProjectSpec)
        and: [ self operator == aMetacelloProjectSpec operator ] ]