printing
classSelectorDictionary
	^ classes 
		inject: (IdentityDictionary new: classes size)
		into: [ :answer :class |
			answer
				at: class put: (self systemDictionary at: class) selectors;
				yourself ]