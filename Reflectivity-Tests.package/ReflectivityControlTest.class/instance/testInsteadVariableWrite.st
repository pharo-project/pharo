tests - instead
testInsteadVariableWrite
	| varNode instance |
	varNode := (ReflectivityExamples >> #exampleAssignment) variableWriteNodes first.
	link := MetaLink new
		metaObject: #context;
		selector: #tempNamed:put:;
		control: #instead;
		arguments: #(#name #newValue).
	varNode link: link.
	self assert: varNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleAssignment = 3.