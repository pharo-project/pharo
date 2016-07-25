serialize/materialize
materializeReferencesOf: anObject with: aDecoder 

	mapping do: [ :index | 
			| reference |
			reference := aDecoder nextEncodedReference.
			index ifNotNil: [
				anObject 
					instVarAt: index
					put: reference ]].