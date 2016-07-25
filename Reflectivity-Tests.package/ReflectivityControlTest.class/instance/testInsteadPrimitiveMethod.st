tests - instead
testInsteadPrimitiveMethod
	| node |
	ReflectivityExamples recompile: #examplePrimitiveMethod.
	node := (ReflectivityExamples >> #examplePrimitiveMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #instead.
	node link: link.
	self assert: node hasMetalink.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: tag isNil.
	ReflectivityExamples new examplePrimitiveMethod.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.