tests - after
testAfterMethodWithTemps
	| methodNode |
	ReflectivityExamples recompile: #exampleAssignment.
	methodNode := (ReflectivityExamples >> #exampleAssignment) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment  = 3.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.