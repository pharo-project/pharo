accessing
variableBindings
	^ (variableBindingsBlock glamourValue: self entity) ifNil: [OrderedCollection new]