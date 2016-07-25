accessing
choose: aToken
	chooseBlock 
		ifNotNil: [self closeChooser.
			previousToken := aToken.
			chooseBlock value: aToken]