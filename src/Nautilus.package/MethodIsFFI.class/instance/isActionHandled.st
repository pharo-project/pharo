order
isActionHandled

	^ ((method literalAt: 1) class == ExternalLibraryFunction) or: [
		method sendsAnySelectorOf: self ffiSelectors
	] 