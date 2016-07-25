tests
testPackagePatternString
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: ''.
	nautilus := Nautilus openOnMethod: compiledMethod.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: 'H'.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: 'anything long'.
	self assertSelectedCompiledMethod.
	nautilus packagePatternString: ''.
	self assertSelectedCompiledMethod.