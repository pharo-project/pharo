tests - before
testBeforeMethodPrimitive
	| methodNode |
	methodNode := (ReflectivityExamples >> #examplePrimitiveMethod) ast.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.	
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new examplePrimitiveMethod class = ByteString.
	self deny: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.	
	self assert: tag = #yes.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	link uninstall.
	self assert: (ReflectivityExamples >> #examplePrimitiveMethod) class = CompiledMethod.
	self assert: (ReflectivityExamples>>#examplePrimitiveMethod) isRealPrimitive.