visiting
visitClassDefinition: definition
	self writeClassDefinition: definition.
	definition hasClassInstanceVariables ifTrue: [self writeMetaclassDefinition: definition].
	definition hasComment ifTrue: [self writeClassComment: definition].