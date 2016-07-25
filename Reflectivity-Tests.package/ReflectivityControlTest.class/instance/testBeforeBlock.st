tests - before
testBeforeBlock
	| blockNode |
	blockNode := (ReflectivityExamples >> #exampleBlock) ast statements first value receiver.
	self assert: blockNode isBlock.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	blockNode link: link.
	self assert: blockNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleBlock) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleBlock == 5.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleBlock) class = CompiledMethod.