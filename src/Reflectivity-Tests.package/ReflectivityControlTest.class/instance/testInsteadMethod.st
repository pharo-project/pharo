tests - instead
testInsteadMethod
	| node |
	node := (ReflectivityExamples >> #exampleMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #instead.
	node link: link.
	self assert: node hasMetalink.
	self assert: (ReflectivityExamples >> #exampleMethod) class = ReflectiveMethod.
	self assert: tag isNil.
	ReflectivityExamples new exampleMethod.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleMethod) class = CompiledMethod.