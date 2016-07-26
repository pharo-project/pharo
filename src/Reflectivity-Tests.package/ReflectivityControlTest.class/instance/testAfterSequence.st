tests - after
testAfterSequence
	| sequence |
	sequence := (ReflectivityExamples >> #exampleMethod) ast body.
	self assert: sequence isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	sequence link: link.
	self assert: sequence hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.