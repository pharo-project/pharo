slots
usesSpecialVariables
    "return true if we define something else than InstanceVariableSlots"
    ^(self usesSpecialSlot 
        or: [ self class usesSpecialSlot ])
        or: [self usesSpecialClassVariables ]