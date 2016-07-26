tests - before
testBeforeLiteralArray
	| literalArray |
	literalArray := (ReflectivityExamples >> #exampleLiteralArray) ast statements first value.
	self assert: literalArray isLiteralArray.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	literalArray link: link.
	self assert: literalArray hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleLiteralArray = #(1).
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteralArray) isQuick.