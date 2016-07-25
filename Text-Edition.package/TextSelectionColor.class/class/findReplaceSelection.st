default accessing
findReplaceSelection
	^ FindReplaceSelection
		ifNil: [FindReplaceSelection := self new
						colorBlock: [self theme currentSettings findReplaceSelectionTextColor]]