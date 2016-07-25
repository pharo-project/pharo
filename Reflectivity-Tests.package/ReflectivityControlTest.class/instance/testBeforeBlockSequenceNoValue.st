tests - before
testBeforeBlockSequenceNoValue
	| sequence |
	sequence := (ReflectivityExamples >> #exampleBlockNoValue) ast statements first value body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	sequence link: link.
	self assert: sequence hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleBlockNoValue) class = ReflectiveMethod.
	self assert: tag isNil.
	ReflectivityExamples new exampleBlockNoValue.
	self assert: tag isNil.
	ReflectivityExamples new exampleBlockNoValue value.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlockNoValue) class = CompiledMethod.