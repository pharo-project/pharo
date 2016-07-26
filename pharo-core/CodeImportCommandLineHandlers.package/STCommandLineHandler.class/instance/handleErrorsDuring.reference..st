installing
handleErrorsDuring: aBlock reference: aReference
	aBlock
		on: Error, OCSemanticWarning, OCSemanticError
		do: [ :e | self handleError: e reference: aReference ]