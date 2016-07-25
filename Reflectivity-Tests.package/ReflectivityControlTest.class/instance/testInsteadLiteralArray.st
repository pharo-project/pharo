tests - instead
testInsteadLiteralArray
	| literalArray |
	literalArray := (ReflectivityExamples >> #exampleLiteralArray) ast statements first value.
	self assert: literalArray isLiteralArray.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #return3.
	literalArray link: link.
	self assert: literalArray hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleLiteralArray = 3.
	self assert: (ReflectivityExamples >> #exampleLiteralArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleLiteralArray) isQuick.