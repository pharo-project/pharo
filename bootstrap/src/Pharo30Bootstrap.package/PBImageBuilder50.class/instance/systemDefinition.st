definition
systemDefinition

	^ systemDefinition ifNil: [ 
		self fetchSystemDefinition.
		systemDefinition ].