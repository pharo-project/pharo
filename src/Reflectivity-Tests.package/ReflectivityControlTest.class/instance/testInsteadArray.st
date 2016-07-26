tests - instead
testInsteadArray
	| literalArray |
	literalArray := (ReflectivityExamples >> #exampleArray) ast statements first value.
	self assert: literalArray isArray.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #return3.
	literalArray link: link.
	self assert: literalArray hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleArray) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleArray = 3.
	self assert: (ReflectivityExamples >> #exampleArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleArray) isQuick.