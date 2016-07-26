tests - creation
testInvalidate
	selector := #exampleMethod.
	(ReflectivityExamples>>#exampleMethod) createTwin. 
	(ReflectivityExamples>>#exampleMethod) invalidate. 
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod. 
	(ReflectivityExamples>>#exampleMethod) reflectiveMethod: nil.
	
	