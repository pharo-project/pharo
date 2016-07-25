tests - return
testReifyReturnValue
	| returnNode instance |
	returnNode := (ReflectivityExamples>>#exampleAssignment) ast body statements last.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(value).
	returnNode link: link.
	self assert: returnNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).