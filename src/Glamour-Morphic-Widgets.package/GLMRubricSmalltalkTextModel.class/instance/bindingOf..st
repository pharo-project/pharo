bindings
bindingOf: aSymbol

	^ self bindingsStrategy cull: self variableBindings cull: aSymbol cull: self