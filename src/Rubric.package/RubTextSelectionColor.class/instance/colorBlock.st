accessing
colorBlock

	^ colorBlock ifNil: [colorBlock := [self theme currentSettings selectionTextColor]]