inline messages factored
emitIf: aMessageNode boolean: aBoolean

	valueTranslator visitNode: aMessageNode receiver.
	
	self emitCondition: aMessageNode arguments boolean: aBoolean