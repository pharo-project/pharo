accessing
superclass
	
	classDefinition superclass ifNil: [ ^ nil ].
	^ PBClassDefinition on: classDefinition superclass environment: bootstrapEnvironment