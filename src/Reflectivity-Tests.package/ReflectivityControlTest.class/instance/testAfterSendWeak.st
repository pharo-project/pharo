tests - after
testAfterSendWeak
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	self assert: sendNode isMessage.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		control: #after;
		option: #(+ optionWeakAfter);
		arguments: #(node).
	sendNode link: link.
	self assert: sendNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	self deny: ((ReflectivityExamples >> #exampleMethod) messages includes: #ensure:).
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.