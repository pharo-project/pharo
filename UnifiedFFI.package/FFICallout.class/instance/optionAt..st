accessing
optionAt: optionName
	^ self options 
		at: optionName 
		ifAbsent: false