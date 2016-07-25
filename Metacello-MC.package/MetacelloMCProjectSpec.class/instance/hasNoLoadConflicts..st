scripting
hasNoLoadConflicts: aMetacelloProjectSpec
  "'projectPackage repositories'"

  ^ (super hasNoLoadConflicts: aMetacelloProjectSpec)
    and: [ self file = aMetacelloProjectSpec file ]