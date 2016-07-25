tests
testCoverageAST
	selector := #exampleMethod.
	
	link := MetaLink new
		metaObject: #node;
		selector: #tagExecuted.

	(ReflectivityExamples>>#exampleMethod) ast nodesDo: [:node | node link: link].

	self deny: (ReflectivityExamples>>#exampleMethod) ast hasBeenExecuted.
	self assert: ReflectivityExamples new exampleMethod = 5.
	self assert: (ReflectivityExamples>>#exampleMethod) ast hasBeenExecuted.
	self assert: (ReflectivityExamples>>#exampleMethod) sendNodes first hasBeenExecuted.