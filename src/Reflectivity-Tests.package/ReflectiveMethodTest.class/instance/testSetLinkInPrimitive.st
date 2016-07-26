tests - links
testSetLinkInPrimitive
	| sendNode link |
	selector := #examplePrimitiveMethod.
	link := MetaLink new.
	sendNode := (ReflectivityExamples>>#examplePrimitiveMethod) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples>>#examplePrimitiveMethod) destroyTwin.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) reflectiveMethod isNil.
	(ReflectivityExamples>>#examplePrimitiveMethod) recompile