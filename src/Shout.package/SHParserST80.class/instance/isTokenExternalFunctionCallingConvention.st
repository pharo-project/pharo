token testing
isTokenExternalFunctionCallingConvention
	| descriptorClass |
	descriptorClass := Smalltalk
				at: #ExternalFunction
				ifAbsent: [].
	descriptorClass
		ifNil: [^ false].
	^ (descriptorClass callingConventionFor: currentToken) notNil