typing support
performCmdActionsWith: aKeyboardEvent shifted: aBoolean 
	| asciiValue actions|
	asciiValue := aKeyboardEvent keyValue + 1.
	actions := aBoolean
				ifTrue: [self class shiftCmdActions]
				ifFalse: [self class cmdActions].
	^ self perform: (actions at: asciiValue) with: aKeyboardEvent