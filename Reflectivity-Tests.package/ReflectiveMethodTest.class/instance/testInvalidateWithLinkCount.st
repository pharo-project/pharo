tests - creation
testInvalidateWithLinkCount
	selector := #exampleMethod.
	(ReflectivityExamples>>#exampleMethod) createTwin. 
	(ReflectivityExamples>>#exampleMethod) invalidate. 
	(ReflectivityExamples>>#exampleMethod) linkCount: 1.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod. 
	self deny: (ReflectivityExamples>>#exampleMethod) reflectiveMethod isNil.
	
	(ReflectivityExamples>>#exampleMethod) reflectiveMethod: nil.