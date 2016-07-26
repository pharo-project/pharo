tests - method
testReifyMethodArgsAfter
	| methodNode instance|
	methodNode := (ReflectivityExamples>>#exampleWithArg:) ast.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		control: #after;
		arguments: #(arguments).
	methodNode link: link.
	self assert: methodNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleWithArg:) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: ((instance exampleWithArg: 3) = 5).
	self assert: (tag = #(3)).
