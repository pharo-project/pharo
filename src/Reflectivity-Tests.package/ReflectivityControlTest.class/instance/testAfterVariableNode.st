tests - after
testAfterVariableNode
	| variableNode |
	variableNode := (ReflectivityExamples >> #exampleGlobalRead) ast statements first value.
	self assert: variableNode isVariable.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	variableNode link: link.
	self assert: variableNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleGlobalRead = GlobalForTesting.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleGlobalRead) isQuick.