tests - after
testAfterLiteral
	| literalNode |
	literalNode := (ReflectivityExamples >> #exampleLiteral) ast statements first value.
	self assert: literalNode isLiteralNode.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	literalNode link: link.
	self assert: literalNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteral == 2.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteral) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteral) isQuick.