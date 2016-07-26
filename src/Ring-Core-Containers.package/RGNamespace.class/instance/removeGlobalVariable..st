removing elements
removeGlobalVariable: anORGlobalVariableDefinition
	"removes anORGlobalVariableDefinition from the globals dictionary if satisfies it #isGlobalVariable"

	anORGlobalVariableDefinition isGlobalVariable ifFalse:[ ^self ].
	self removeElement: anORGlobalVariableDefinition from: self globalVariables