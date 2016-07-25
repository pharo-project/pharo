construction
add: aLabelString selector: aSymbol argument: arg

	^ self add: aLabelString
		target: defaultTarget
		selector: aSymbol
		argumentList: (Array with: arg)
