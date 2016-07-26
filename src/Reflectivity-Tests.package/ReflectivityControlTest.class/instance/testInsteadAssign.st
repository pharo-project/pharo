tests - instead
testInsteadAssign
	| assignmentNode instance |
	assignmentNode := (ReflectivityExamples >> #exampleAssignment) assignmentNodes first.
	self assert: assignmentNode isAssignment.
	link := MetaLink new
		metaObject: #context;
		selector: #tempNamed:put:;
		control: #instead;
		arguments: #(#name #newValue).
	assignmentNode link: link.
	self assert: assignmentNode hasMetalink.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	instance := ReflectivityExamples new.
	self assert: instance exampleAssignment = 3.