*System-Object Events
hasActionForEvent: anEventSelector
    "Answer true if there is an action associated with anEventSelector"

    ^(self actionForEvent: anEventSelector) notNil