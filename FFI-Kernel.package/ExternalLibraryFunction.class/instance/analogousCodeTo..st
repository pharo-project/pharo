comparing
analogousCodeTo: anObject
	^(anObject isKindOf: ExternalLibraryFunction)
	and: [flags = anObject flags
	and: [argTypes = anObject argTypes
	and: [name = anObject name
	and: [module = anObject module
	and: [errorCodeName = anObject errorCodeName]]]]]