tests - creation
testCreateReflectiveMethodAndInvalidate
	| compiledMethod reflectiveMethod |
	selector := #exampleMethod.
	compiledMethod := ReflectivityExamples>>#exampleMethod.
	reflectiveMethod := ReflectiveMethod on: compiledMethod.
	self assert: reflectiveMethod class = ReflectiveMethod.
	compiledMethod reflectiveMethod: reflectiveMethod. 
	self assert: (compiledMethod reflectiveMethod = reflectiveMethod).
	
	compiledMethod invalidate.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod. 
	(ReflectivityExamples>>#exampleMethod) reflectiveMethod: nil.
	
	