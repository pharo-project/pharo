*System-Object Events
actionForEvent: anEventSelector
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	| actions |
	actions := self actionMap
		at: anEventSelector asSymbol
		ifAbsent: [nil].
	actions ifNil: [^nil].
	^ actions asMinimalRepresentation