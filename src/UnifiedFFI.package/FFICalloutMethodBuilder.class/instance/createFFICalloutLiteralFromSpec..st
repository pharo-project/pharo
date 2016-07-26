private factory
createFFICalloutLiteralFromSpec: functionSpec
	| externalFunction |
	
	externalFunction := ExternalLibraryFunction
		name: functionSpec functionName
		module: self moduleName
		callType: self callType
		returnType: functionSpec returnType externalTypeWithArity
		argumentTypes: (functionSpec arguments collect: #externalTypeWithArity).
	
	functionResolutionStrategies
		detect: [ :each | each isApplicableFor: self requestor ]
		ifFound: [ :each | each resolve: externalFunction ].
		
	^ externalFunction