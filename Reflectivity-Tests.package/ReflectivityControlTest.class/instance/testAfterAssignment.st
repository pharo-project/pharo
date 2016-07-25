tests - after
testAfterAssignment
	| assignmentNode |
	assignmentNode := (ReflectivityExamples >> #exampleAssignment) assignmentNodes first.
	self assert: assignmentNode isAssignment.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec;
		control: #after.
	assignmentNode link: link.
	self assert: assignmentNode hasMetalinkAfter.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment = 3.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.