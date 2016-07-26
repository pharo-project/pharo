serialize/materialize
materializeReferencesVariablePartOf: anObject with: aDecoder

	1 to: anObject basicSize 
		do: [ :index | anObject basicAt: index put: aDecoder nextEncodedReference ]