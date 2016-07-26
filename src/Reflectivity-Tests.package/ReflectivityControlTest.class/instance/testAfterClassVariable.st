tests - after - variables
testAfterClassVariable
	| classVar |
	classVar := ReflectivityExamples classVariableNamed: #ClassVar.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	classVar link: link.
	self assert: classVar hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleClassVarRead  = #AClassVar.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleClassVarRead) class = CompiledMethod.
	self deny: (ReflectivityExamples >> #exampleClassVarRead) isQuick.