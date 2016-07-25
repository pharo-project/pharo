private
matches: aResolvedReference
	^ (super matches: aResolvedReference) and: [ constraintBlock value: aResolvedReference ]