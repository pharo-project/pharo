tests - variablenodes
testAccessTemp
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleAssignment) ast body statements second.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: {RBVariableNode named: #temp}.
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).