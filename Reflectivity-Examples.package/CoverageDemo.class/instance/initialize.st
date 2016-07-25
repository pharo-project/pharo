initialization
initialize

	coverageCode := [ReflectivityExamples new exampleMethod].
	compiledMethods := OrderedCollection new.
	compiledMethods add: ReflectivityExamples>>#exampleMethod.
	
	super initialize.