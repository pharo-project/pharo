accessing
actionMapFor: anObject

    ^self actionMaps
        at: anObject
        ifAbsent: [self createActionMap]