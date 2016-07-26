adding elements
addGlobalVariable: anORGlobalVariableDefinition
	"adds anORGlobalVariableDefinition to the globals dictionary if satisfies it #isGlobalVariable"

	anORGlobalVariableDefinition isGlobalVariable ifFalse:[ ^self ].
	self addElement: anORGlobalVariableDefinition in: self globalVariables