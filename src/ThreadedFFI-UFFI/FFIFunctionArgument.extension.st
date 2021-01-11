Extension { #name : #FFIFunctionArgument }

{ #category : #'*ThreadedFFI-UFFI' }
FFIFunctionArgument >> tfExternalTypeWithArity [
	
	^ self resolvedType tfExternalTypeWithArity
]
