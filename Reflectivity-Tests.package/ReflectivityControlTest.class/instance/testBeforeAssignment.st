tests - before
testBeforeAssignment
	| assignmentNode |
	assignmentNode := (ReflectivityExamples >> #exampleAssignment) assignmentNodes first.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	assignmentNode link: link.
	self assert: assignmentNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment = 3.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.