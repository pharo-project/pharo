tests - simple
testReificationError
	| link |
	link := MetaLink new 
		metaObject: #unknown.
	self should: [ (ReflectivityExamples>>#exampleMethod) ast link: link ] raise: Error.
	self should: [ (ReflectivityExamples classVariableNamed: #ClassVar) link: link ] raise: Error.