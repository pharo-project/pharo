tests - conditions
testConditionDisableEnableNoArguments
"check that a link is not executed, if the condition is disabled (link>>#disable)."
"and  that it is executed again if the condition is enabled again (link>>#enable"
	| sendNode |
	sendNode := (ReflectivityExamples >> #exampleMethod) sendNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec:;
		condition: [ true ];
		arguments: #(#node).
	sendNode link: link.
	self assert: sendNode hasMetalinkBefore.
	self assert: tag isNil.
	ReflectivityExamples new exampleMethod.
	self assert: tag class equals: RBMessageNode.
	tag := nil.
	link disable.
	ReflectivityExamples new exampleMethod.
	self assert: tag isNil.
	link enable.
	ReflectivityExamples new exampleMethod.
	self assert: tag class equals: RBMessageNode.
