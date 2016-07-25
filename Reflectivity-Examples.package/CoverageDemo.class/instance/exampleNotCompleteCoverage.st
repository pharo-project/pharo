example
exampleNotCompleteCoverage

	self coverageCode: [ ReflectivityExamples new exampleIfTrueIfFalse. ].
	self compiledMethods add: ReflectivityExamples>>#exampleIfTrueIfFalse.
	
	self openWithSpec.