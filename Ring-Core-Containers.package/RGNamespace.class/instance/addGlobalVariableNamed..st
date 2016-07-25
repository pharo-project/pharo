adding elements
addGlobalVariableNamed: globalVariableName
	"creates a RGGlobalVariableDefinition with the given name.
	It's verified that a pool with that name does not exist"

	(self includesGlobalVariableNamed: globalVariableName) ifTrue: [ ^self ].
	self addGlobalVariable: (RGGlobalVariableDefinition named: globalVariableName).