printing
metaClassSelectorDictionary
	^ metaClasses 
		inject: (IdentityDictionary new: metaClasses size)
		into: [ :answer :class |
			answer
				at: class put: (self systemDictionary at: class) class selectors;
				yourself ]