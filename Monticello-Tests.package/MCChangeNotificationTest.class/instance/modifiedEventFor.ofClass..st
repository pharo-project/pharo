events
modifiedEventFor: aSelector ofClass: aClass
	| method |
	method := aClass compiledMethodAt: aSelector.
	^ MethodModified 
		methodChangedFrom: method
		to: method
		oldProtocol: nil
