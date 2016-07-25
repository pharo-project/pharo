default accessing
secondarySelection
	^ SecondarySelection
		ifNil: [SecondarySelection := self new
						colorBlock: [self theme currentSettings secondarySelectionTextColor]] 