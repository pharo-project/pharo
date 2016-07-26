accessing
orientation: aSymbol
	((aSymbol == #horizontal) | (aSymbol == #vertical))
		ifFalse: [ Error signal: 'orientation must be either #horizontal or #vertical' ]
		ifTrue: [ orientation := aSymbol ]