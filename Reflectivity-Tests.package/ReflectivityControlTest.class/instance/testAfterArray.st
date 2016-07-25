tests - after
testAfterArray
	| arrayNode |
	arrayNode := (ReflectivityExamples >> #exampleArray) ast statements first value.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	arrayNode link: link.
	self assert: arrayNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleArray) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleArray isArray.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleArray) class = CompiledMethod.