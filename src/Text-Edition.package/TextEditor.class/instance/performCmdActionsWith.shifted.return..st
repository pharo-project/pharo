typing support
performCmdActionsWith: aKeyboardEvent shifted: aBoolean return: return
	| asciiValue actions action|
	asciiValue := aKeyboardEvent keyValue + 1.
	actions := aBoolean
				ifTrue: [self class shiftCmdActions]
				ifFalse: [self class cmdActions].
	action := (actions at: asciiValue).
	action == #noop: ifTrue: [ ^ false ].
	return value: ( self perform: action with: aKeyboardEvent).