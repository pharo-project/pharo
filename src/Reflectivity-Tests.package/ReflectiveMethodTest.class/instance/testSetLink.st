tests - links
testSetLink
	| sendNode link |
	selector := #exampleMethod.
	link := MetaLink new.
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	sendNode removeLink: link.
	(ReflectivityExamples>>#exampleMethod) destroyTwin.
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#exampleMethod) reflectiveMethod isNil.
	