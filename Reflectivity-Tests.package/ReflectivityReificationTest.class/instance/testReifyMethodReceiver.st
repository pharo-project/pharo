tests - method
testReifyMethodReceiver
	| sendNode instance|
	sendNode := (ReflectivityExamples>>#exampleMethod) ast.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(receiver).
	sendNode link: link.
	self assert: sendNode hasMetalink.
	self assert: (ReflectivityExamples>>#exampleMethod) class = ReflectiveMethod.
	self assert: (tag isNil).
	instance := ReflectivityExamples new.
	self assert: (instance exampleMethod = 5).
	self assert: (tag = instance).
