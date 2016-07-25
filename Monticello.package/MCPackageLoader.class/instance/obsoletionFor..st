private
obsoletionFor: aDefinition
	^ obsoletions at: aDefinition ifAbsent: [nil]