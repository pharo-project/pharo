*System-Object Events
actionForEvent: anEventSelector
ifAbsent: anExceptionBlock
    "Answer the action to be evaluated when <anEventSelector> has been triggered."

	| actions |
	actions := self actionMap
		at: anEventSelector asSymbol
		ifAbsent: [nil].
	actions ifNil: [^anExceptionBlock value].
	^ actions asMinimalRepresentation