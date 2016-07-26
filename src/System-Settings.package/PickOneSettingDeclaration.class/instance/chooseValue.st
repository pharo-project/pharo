user interface
chooseValue
	| chosen |
	chosen := UIManager default
				chooseFrom: self domainValuesLabels
				values: self domainValues
				title: self label translated, ' choices' translated.
	chosen
		ifNotNil: [self realValue: chosen realValue]