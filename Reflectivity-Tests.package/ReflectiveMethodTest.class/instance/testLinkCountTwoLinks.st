tests - links
testLinkCountTwoLinks
	| sendNode link1 link2 |
	selector := #exampleMethod.
	
	link1 := MetaLink new. 
	link2 := MetaLink new. 
	
	sendNode := (ReflectivityExamples>>#exampleMethod) ast body statements first value.
	sendNode link: link1.
	self assert:  (ReflectivityExamples>>#exampleMethod) linkCount = 1.
	sendNode link: link2.
	self assert: (ReflectivityExamples>>#exampleMethod) linkCount = 2.
	sendNode removeLink: link1.
	self assert: (ReflectivityExamples>>#exampleMethod) linkCount = 1.
	sendNode removeLink: link2.	
	self assert: (ReflectivityExamples>>#exampleMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#exampleMethod) reflectiveMethod isNil.
	