scripting
hasNoLoadConflicts: aMetacelloProjectSpec
  "'projectPackage repositories'"

  ^ (super hasNoLoadConflicts: aMetacelloProjectSpec)
    and: [ 
      (self repositories isEmpty or: [ aMetacelloProjectSpec repositories isEmpty ])
        or: [ self repositories hasNoLoadConflicts: aMetacelloProjectSpec repositories ] ]