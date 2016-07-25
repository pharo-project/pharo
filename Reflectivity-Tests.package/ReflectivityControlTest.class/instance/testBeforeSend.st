tests - before
testBeforeSend
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.