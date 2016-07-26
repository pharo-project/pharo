tests - variablenodes
testReifyTempName
	| varNode instance |
	varNode := (ReflectivityExamples>>#exampleAssignment) ast body statements first variable.
	link := MetaLink new 
		metaObject: self; 
		selector: #tagExec:;
		arguments: #(name).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (tag isNil).
	instance := ReflectivityExamples new .
	self assert: (instance exampleAssignment = 3).
	self assert: (tag ='temp').