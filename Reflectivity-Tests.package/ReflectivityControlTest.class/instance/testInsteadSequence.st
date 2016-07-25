tests - instead
testInsteadSequence
	| seqNode |
	seqNode := (ReflectivityExamples >> #exampleAssignment) ast body.
	self assert: seqNode isSequence.
	link := MetaLink new
		metaObject: self;
		control: #instead;
		selector: #tagExec.
	self assert: (seqNode isKindOf: RBSequenceNode).
	seqNode link: link.
	self assert: seqNode hasMetalinkInstead.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	ReflectivityExamples new exampleAssignment.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.