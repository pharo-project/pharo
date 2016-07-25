default accessing
primarySelection
	^ PrimarySelection
		ifNil: [ 
			PrimarySelection := self new
				colorBlock: [ self theme currentSettings selectionTextColor ];
				backgroundColorBlock: [ self theme currentSettings selectionColor ];
				yourself ]