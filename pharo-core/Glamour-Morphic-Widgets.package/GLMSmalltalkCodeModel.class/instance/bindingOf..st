accessing
bindingOf: aSymbol
	^ self bindingsStrategy cull: self variableBindings cull: aSymbol