*System-Object Events
removeActionsForEvent: anEventSelector

    | map |
    map := self actionMap.
    map removeKey: anEventSelector asSymbol ifAbsent: [].
    map isEmpty
        ifTrue: [self releaseActionMap]