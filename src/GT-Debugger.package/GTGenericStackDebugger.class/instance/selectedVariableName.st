accessing
selectedVariableName
	^ self cache at: #selectedVariableName ifAbsent: [ nil ]