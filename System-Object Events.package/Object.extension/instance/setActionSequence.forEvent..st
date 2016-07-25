*System-Object Events
setActionSequence: actionSequence
forEvent: anEventSelector

    | action |
    action := actionSequence asMinimalRepresentation.
    action 
	  ifNil:
            [self removeActionsForEvent: anEventSelector]
        ifNotNil:
            [self updateableActionMap
                at: anEventSelector asSymbol
                put: action]