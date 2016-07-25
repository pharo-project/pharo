tests - instead
testInsteadVariableReadGlobal
	| varNode instance |
	varNode := (ReflectivityExamples >> #exampleGlobalRead) variableReadNodes first.
	link := MetaLink new
		metaObject: #class;
		selector: #bindingOf:;
		control: #instead;
		arguments: #(#name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleGlobalRead) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleGlobalRead value = GlobalForTesting.