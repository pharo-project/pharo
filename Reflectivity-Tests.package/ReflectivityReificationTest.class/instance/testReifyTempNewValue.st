tests - variablenodes
testReifyTempNewValue
	| varNode instance|
	varNode := (ReflectivityExamples>>#exampleAssignment) ast body statements first variable.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(newValue).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag = 3).