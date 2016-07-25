tests - creation
testInvalidateNoJit
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) createTwin. 
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass)  invalidate. 
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass)  class = CompiledMethod.
	self assert: ReflectivityExamples exampleMethodWithMetaLinkOptionsViaClass = 5.
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) class = CompiledMethod. 
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) reflectiveMethod: nil.
	ReflectivityExamples class recompile: #exampleMethodWithMetaLinkOptionsViaClass.
	