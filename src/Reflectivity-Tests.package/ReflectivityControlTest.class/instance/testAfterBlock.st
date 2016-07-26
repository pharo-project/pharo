tests - after
testAfterBlock
	| blockNode |
	blockNode := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver.
	self assert: blockNode isBlock.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	blockNode link: link.
	self assert: blockNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleBlock == 5.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.