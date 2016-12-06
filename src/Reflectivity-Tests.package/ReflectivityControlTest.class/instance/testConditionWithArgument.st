tests - conditions
testConditionWithArgument
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		condition: [ :node | node == sendNode ] arguments: #(#node);
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag class = RBMessageNode.
	link condition: [ :node | node == 5 ].
	tag := nil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag isNil.