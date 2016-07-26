testing
isAbstract: aClass 
	^(aClass isMeta or: 
			[(self literals includes: aClass name)
				or: [self literals includes: (Smalltalk globals associationAt: aClass name)]])
		not