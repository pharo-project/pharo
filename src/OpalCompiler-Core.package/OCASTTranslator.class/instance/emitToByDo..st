inline messages
emitToByDo: aMessageNode 

	| step |
	
	step := aMessageNode arguments second.
	step isLiteralNode ifFalse: [self error: 'should not have been inlined'].
	step := step value.

	self emitToDo: aMessageNode step: step