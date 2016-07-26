tests
testOpenLastValueIsNil
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: nil.
	nautilus := Nautilus open.
	nautilus packagePatternString: 'H'.
	nautilus packagePatternString: 'anything long'.
	nautilus packagePatternString: ''.
