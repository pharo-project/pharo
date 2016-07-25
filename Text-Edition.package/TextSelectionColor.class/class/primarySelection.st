default accessing
primarySelection
	^ PrimarySelection
		ifNil: [PrimarySelection := self new colorBlock: [ self theme currentSettings selectionTextColor]]