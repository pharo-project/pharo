private
resolveTextLinkFor: aVariableNode

	aVariableNode binding isGlobalVariable
		ifTrue: [ ^ TextClassLink className: aVariableNode name ].
		
	^ TextVariableLink variableName: aVariableNode name