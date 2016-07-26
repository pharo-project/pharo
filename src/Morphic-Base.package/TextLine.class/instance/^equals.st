comparing
= line

	self species = line species
		ifTrue: [^((firstIndex = line first and: [lastIndex = line last])
				and: [internalSpaces = line internalSpaces])
				and: [paddingWidth = line paddingWidth]]
		ifFalse: [^false]