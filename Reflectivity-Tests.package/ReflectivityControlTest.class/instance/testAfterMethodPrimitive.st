tests - after
testAfterMethodPrimitive
	| methodNode |
	ReflectivityExamples recompile: #examplePrimitiveMethod.
	methodNode := (ReflectivityExamples >> #examplePrimitiveMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new examplePrimitiveMethod class = ByteString.
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.