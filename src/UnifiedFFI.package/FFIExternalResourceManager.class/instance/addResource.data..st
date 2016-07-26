external resource management
addResource: anObject data: resourceData
	registry 
		add: anObject 
		executor: (FFIExternalResourceExecutor new 
			resourceClass: anObject class
			data: resourceData)