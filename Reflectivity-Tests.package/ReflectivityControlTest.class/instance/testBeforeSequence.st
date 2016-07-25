tests - before
testBeforeSequence
	| seqNode |
	seqNode := (ReflectivityExamples >> #exampleAssignment) ast body.
	self assert: seqNode isSequence.
	link := MetaLink new
		metaObject: self;
		selector: #tagExec.
	self assert: (seqNode isKindOf: RBSequenceNode).
	seqNode link: link.
	self assert: seqNode hasMetalinkBefore.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = ReflectiveMethod.
	self assert: tag isNil.
	self assert: ReflectivityExamples new exampleAssignment = 3.
	self assert: tag = 'yes'.
	self assert: (ReflectivityExamples >> #exampleAssignment) class = CompiledMethod.