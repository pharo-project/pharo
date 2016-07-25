*System-Object Events
removeActionsWithReceiver: anObject
forEvent: anEventSelector

    self
        removeActionsSatisfying:
            [:anAction |
            anAction receiver == anObject]
        forEvent: anEventSelector