tests - before
testbeforeArray
	| arrayNode |
	arrayNode := (ReflectivityExamples >> #exampleArray) ast statements first value.
	self assert: arrayNode isArray.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	arrayNode link: link.
	self assert: arrayNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleArray) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleArray isArray.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleArray) class = CompiledMethod.