default accessing
oppositeDelimiterSelection
	^ OppositeDelimiterSelection
		ifNil: [ 
			OppositeDelimiterSelection := self new
				colorBlock: [ self theme currentSettings selectionTextColor ];
				backgroundColorBlock: [ self theme currentSettings secondarySelectionColor ];
				yourself ]