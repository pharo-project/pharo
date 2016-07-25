visiting
visitMethodDefinition: definition
	self writeMethodPreamble: definition.
	self writeMethodSource: definition.
	self writeMethodPostscript.
	self writeInitializers ifTrue: [ 
		self writeMethodInitializer: definition ].