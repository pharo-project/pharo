tests - variablenodes
testReifyGlobalName
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleGlobalRead) ast body statements last value.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleGlobalRead = GlobalForTesting).
	self assert: (tag = #GlobalForTesting).