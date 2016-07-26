tests - after
testAfterBlockSequence
	| sequence |
	sequence := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	sequence link: link.
	self assert: sequence hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleBlock == 5.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.