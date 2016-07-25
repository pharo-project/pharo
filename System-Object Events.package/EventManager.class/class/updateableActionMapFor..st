accessing
updateableActionMapFor: anObject

    ^self actionMaps
        at: anObject
        ifAbsentPut: [self createActionMap]