scripting
compareEqual: aMetacelloProjectSpec
    "'projectPackage repositories'"

    ^ (super compareEqual: aMetacelloProjectSpec)
        and: [ self repositories compareEqual: aMetacelloProjectSpec repositories ]