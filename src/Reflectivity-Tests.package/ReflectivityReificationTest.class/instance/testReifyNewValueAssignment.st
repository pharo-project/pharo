tests - assignment
testReifyNewValueAssignment
	| varNode instance|
	varNode := (ReflectivityExamples>>#exampleAssignment) assignmentNodes first.
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
