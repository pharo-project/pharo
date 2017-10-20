api - context
selectedVariableName
	^selectedVariable ifNil: [ super selectedVariableName ] ifNotNil:   [selectedVariable]