running
setUp
	super setUp.
	self compiledMethod: (NautilusUI>>#addClassIn:).
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.