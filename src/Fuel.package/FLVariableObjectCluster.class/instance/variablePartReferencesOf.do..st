analyzing
variablePartReferencesOf: anObject do: aBlock 

	1 to: anObject basicSize 
		do: [ :index | aBlock value: (anObject basicAt: index) ]