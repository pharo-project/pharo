tests - instead
testInsteadArray
	| arrayNode |
	arrayNode := (ReflectivityExamples >> #exampleArray) ast statements first value.
	self assert: arrayNode isDynamicArray.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #return3.
	arrayNode link: link.
	self assert: arrayNode hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleArray) class = ReflectiveMethod.
	self assert: ReflectivityExamples new exampleArray = 3.
	self assert: (ReflectivityExamples >> #exampleArray) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleArray) isQuick.