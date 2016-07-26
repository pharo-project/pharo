tests
testOpenLastValueNotNil
	lastValue := SharedPackageFilterPatternsHolder instance lastValue.
	SharedPackageFilterPatternsHolder instance lastValue: 'K'.
	nautilus := Nautilus open.
	nautilus packagePatternString: 'H'.
	nautilus packagePatternString: 'anything long'.
	nautilus packagePatternString: ''.
