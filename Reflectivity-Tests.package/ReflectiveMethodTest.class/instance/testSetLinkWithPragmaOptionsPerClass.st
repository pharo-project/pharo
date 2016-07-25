tests - links
testSetLinkWithPragmaOptionsPerClass
	| sendNode link |
	link := MetaLink new.
	sendNode := (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) ast body statements first value.
	sendNode link: link.
	self assert: (sendNode links includes: link).
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) class = CompiledMethod.
	sendNode removeLink: link.
	(ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) destroyTwin.
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) class = CompiledMethod.
	self assert: (ReflectivityExamples class>>#exampleMethodWithMetaLinkOptionsViaClass) reflectiveMethod isNil.
	ReflectivityExamples class recompile: #exampleMethodWithMetaLinkOptionsViaClass