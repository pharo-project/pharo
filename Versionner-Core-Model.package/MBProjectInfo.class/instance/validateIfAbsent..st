accessing
validateIfAbsent: absentBlock

	^MetacelloToolBox validateConfiguration: (Smalltalk 
		at: self packageName asSymbol 
		ifAbsent: absentBlock).
