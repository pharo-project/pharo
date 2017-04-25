stack parameter classification
stackValueParameterClass
	^ value isFloat ifTrue: [#float] ifFalse: [#integer]
