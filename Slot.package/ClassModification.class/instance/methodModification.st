accessing
methodModification
	^ methodModification ifNil: [
		methodModification := MethodModification new: target classLayout fieldSize.
		self buildModificationMapFor: methodModification ]