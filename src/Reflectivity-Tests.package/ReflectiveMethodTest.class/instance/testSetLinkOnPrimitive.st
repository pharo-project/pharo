tests - links
testSetLinkOnPrimitive
	| methodNode link |
	selector := #examplePrimitiveMethod.
	link := MetaLink new.
	methodNode := (ReflectivityExamples>>#examplePrimitiveMethod) ast.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode link: link.
	self assert: (methodNode links includes: link).
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	self deny: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode removeLink: link.
	(ReflectivityExamples>>#examplePrimitiveMethod) destroyTwin.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) reflectiveMethod isNil.
	