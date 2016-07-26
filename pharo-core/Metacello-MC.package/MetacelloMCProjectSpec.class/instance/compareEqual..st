scripting
compareEqual: aMetacelloProjectSpec
    "'projectPackage repositories'"

    ^ (super compareEqual: aMetacelloProjectSpec) and: [ self file = aMetacelloProjectSpec file ]