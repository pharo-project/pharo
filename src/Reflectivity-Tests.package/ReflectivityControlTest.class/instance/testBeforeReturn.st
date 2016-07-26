tests - before
testBeforeReturn
	| returnNode |
	returnNode := (ReflectivityExamples >> #exampleLiteral) ast statements first.
	self assert: returnNode isReturn.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	returnNode link: link.
	self assert: returnNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteral == 2.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteral) isQuick.