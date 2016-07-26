tests - before
testBeforeVariable
	| variableNode |
	variableNode := (ReflectivityExamples >> #exampleGlobalRead) ast statements first value.
	self assert: variableNode isVariable.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	variableNode link: link.
	self assert: variableNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleGlobalRead = GlobalForTesting.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleGlobalRead) isQuick.