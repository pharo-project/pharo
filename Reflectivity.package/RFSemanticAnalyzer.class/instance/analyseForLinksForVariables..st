visiting
analyseForLinksForVariables: aVariableNode
	| variable generator |
	variable := aVariableNode binding variable.
	variable hasMetalink
		ifFalse: [ ^ self ].
	generator := HookGeneratorForVariables entity: variable node: aVariableNode.
	self analyseForLinks: variable generator: generator.

