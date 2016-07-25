recording ranges
rangeType: aSymbol start: s end: e 
	^ ranges
		add: (SHRange
				start: s
				end: e
				type: aSymbol)